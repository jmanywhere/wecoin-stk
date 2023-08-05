// SPDX-License-Identifier: MIT
pragma solidity 0.8.19;

import "openzeppelin/token/ERC20/IERC20.sol";
import "./interfaces/ISasWecoin.sol";
import "openzeppelin/utils/math/Math.sol";
// TODO REMOVE
import "forge-std/console.sol";

error SasWecoin__NoBalanceToWithdraw();
error SasWecoin__NothingToClaim();
error SasWecoin__InvalidDepositAmount();

contract SasWecoin is ISasWecoin {
    mapping(address => UserInfo) public users;
    mapping(uint => EpochInfo) public epochs;

    IERC20 public WECOIN;

    uint public totalRewards;
    uint public immutable stakingStartTime;
    uint public prevTimestamp;
    uint public totalBaseStakingPower;
    uint public totalBonusStakingPower;
    uint public accumulatedRewardsPerStakingPower;
    uint public rewardPenalty = 5;
    uint private lastAccumulatedEpoch;
    uint private lastTotalReward;
    // This magnifier keeps all decimals when doing integer division
    // Not concerned about overflow since it is only used internally and
    // WECOIN has 18 decimals + a max supply of 10B
    // 29 decimal places total
    // 29 + 20 = 49 decimal places total
    // considering uint256 has a max of 77 decimal places
    // THE MAX VALUE  where MAGNIFIER is used is
    // (rewardOffset * WEEKLY_EMISSIONS * diff * MAGNIFIER)
    //     29               2               10      20    <= DECIMALS USED
    // MAX is 61 DECIMAL PLACES
    uint private constant MAGNIFIER = 1e20;
    uint private constant EPOCH_DURATION = 1 weeks;
    uint public constant SQRT_ADJUSTMENT = 100_00;
    uint private constant DAILY_EMISSIONS = 4;
    uint private constant WEEKLY_EMISSIONS = 28;
    uint private constant EMISSIONS_BASE = 100_00;
    uint private constant INT_PERCENTAGE_BASE = 100;

    //---------------------------------------------------
    //                  CONSTRUCTOR
    //---------------------------------------------------
    constructor(address _wecoin, uint _stakingStartTime) {
        WECOIN = IERC20(_wecoin);
        stakingStartTime = _stakingStartTime;
        prevTimestamp = _stakingStartTime;
    }

    function deposit(uint _amount, uint256 _weeksLocked) external {
        if (_amount == 0) {
            revert SasWecoin__InvalidDepositAmount();
        }
        UserInfo storage user = users[msg.sender];
        // Lock or claim rewards
        // NOTE here is where offsetpoints are updated 1 time
        _claimAndLock(msg.sender);
        // get current epoch
        uint currentEpoch = _currentEpoch();
        // Deposit amount
        uint fullAmount = user.depositAmount + _amount;
        uint bonusStakingPower = user.bonusAmount;
        totalBonusStakingPower -= bonusStakingPower;
        totalBaseStakingPower += _amount;
        user.lastAction = block.timestamp;
        user.depositAmount = fullAmount;
        // need to check if there is a previous locking period and extend it.
        if (
            _weeksLocked > 0 ||
            (currentEpoch > 0 && user.endLockEpoch >= currentEpoch)
        ) {
            if (user.endLockEpoch >= currentEpoch) {
                epochs[user.endLockEpoch + 1]
                    .totalBonusStakingPowerAdjustment -= user.bonusAmount;
                user.endLockEpoch += _weeksLocked;
                user.lockDuration = user.endLockEpoch - currentEpoch;
            } else {
                user.endLockEpoch = _weeksLocked + currentEpoch;
                user.lockDuration = _weeksLocked;
            }

            uint multiplier = calculateMultiplier(user.lockDuration);
            bonusStakingPower = (fullAmount * multiplier) / SQRT_ADJUSTMENT;
            bonusStakingPower -= fullAmount;
            // Set the adjustment needed when the end epoch arrives
            epochs[user.endLockEpoch + 1]
                .totalBonusStakingPowerAdjustment += bonusStakingPower;

            user.bonusAmount = bonusStakingPower;
            totalBonusStakingPower += bonusStakingPower;
        } else {
            user.bonusAmount = 0;
        }

        // adjust offsetPoints here as well
        user.offsetPoints =
            ((fullAmount + bonusStakingPower) *
                accumulatedRewardsPerStakingPower) /
            MAGNIFIER;
        // Transfer in WECOIN
        WECOIN.transferFrom(msg.sender, address(this), _amount);
    }

    function claimOrLock() external {
        UserInfo storage user = users[msg.sender];
        if (user.depositAmount == 0) revert SasWecoin__NothingToClaim();
        _claimAndLock(msg.sender);
    }

    function updateAccumulator() external {
        _updateAccumulator();
    }

    function addTokenRewards(uint amount) external {
        WECOIN.transferFrom(msg.sender, address(this), amount);
        totalRewards += amount;
        lastTotalReward += amount;
    }

    function withdraw() external {
        // Update all values
        _claimAndLock(msg.sender);

        UserInfo storage user = users[msg.sender];
        if (user.depositAmount == 0) {
            revert SasWecoin__NoBalanceToWithdraw();
        }
        uint currentEpoch = _currentEpoch();
        uint withdrawableAmount = user.depositAmount;
        uint penalty;
        if (user.endLockEpoch > 0 && currentEpoch <= user.endLockEpoch) {
            // Calculate and apply penalties
            uint epochDiff = (user.endLockEpoch + 1) *
                EPOCH_DURATION +
                stakingStartTime -
                block.timestamp;
            uint threshold = (user.lockDuration * EPOCH_DURATION) / 2;
            // Penalty of 50% threshold
            if (epochDiff > threshold) {
                // Principal penalty
                penalty =
                    (withdrawableAmount * rewardPenalty) /
                    INT_PERCENTAGE_BASE;
                withdrawableAmount -= penalty;

                emit WithdrawPenalty(msg.sender, penalty, user.lockedRewards);
                // Reward penalty fully penalized
                penalty += user.lockedRewards;
            } else {
                // MATH penalty
                penalty = (180000 * threshold) - (epochDiff * 1600);
                penalty = (user.lockedRewards * penalty) / (threshold * 1000);
                withdrawableAmount += user.lockedRewards - penalty;
                emit WithdrawPenalty(msg.sender, 0, penalty);
            }
            totalRewards += penalty;
            lastTotalReward += penalty;
            // Adjust the next epochs adjustment since this is an early withdrawal
            epochs[user.endLockEpoch + 1]
                .totalBonusStakingPowerAdjustment -= user.bonusAmount;
            totalBonusStakingPower -= user.bonusAmount;
        }

        totalBaseStakingPower -= user.depositAmount;
        // Reset all values
        users[msg.sender] = UserInfo({
            depositAmount: 0,
            bonusAmount: 0,
            offsetPoints: 0,
            lastAction: block.timestamp,
            endLockEpoch: 0,
            lockDuration: 0,
            lockedRewards: 0
        });
        emit Withdraw(msg.sender, withdrawableAmount);
        // Transfer out WECOIN
        if (withdrawableAmount > 0)
            WECOIN.transfer(msg.sender, withdrawableAmount);
    }

    //-----------------------------------------------------------------------------------
    // Internal & Private functions
    //-----------------------------------------------------------------------------------
    /**
     * Adjust the current accumulator to the current epoch and timestamp.
     * @dev This function checks 2 different scenarios:
     * 1. If the current epoch is the same as the last accumulated epoch, it will update the accumulator with the reward difference between the last update and the current timestamp.
     * 2. If the current epoch is greater than the last accumulated epoch, it will update the accumulator with the reward difference between the last epoch, any in between epochs(unlikely) and the current epoch.
     */
    function _updateAccumulator() internal {
        // Make sure updates only happen after initial time
        if (block.timestamp <= prevTimestamp) return;

        uint currentEpoch = _currentEpoch();

        uint rewardOffset = lastTotalReward;
        uint epochEmissions = 0;
        uint baseStakingPower = _latestStakingPower(lastAccumulatedEpoch);
        // NOTE Single EPOCH update
        if (currentEpoch == lastAccumulatedEpoch) {
            if (block.timestamp > prevTimestamp) {
                uint diff = block.timestamp - prevTimestamp;
                // Get the next accumulation of rewards
                diff =
                    (rewardOffset * WEEKLY_EMISSIONS * diff * MAGNIFIER) /
                    (EPOCH_DURATION * EMISSIONS_BASE * baseStakingPower);

                accumulatedRewardsPerStakingPower += diff;

                epochs[currentEpoch]
                    .finalEpochAccumulation = accumulatedRewardsPerStakingPower;

                prevTimestamp = block.timestamp;
            }
            return;
        }

        // NOTE Multiple Epoch diff update
        uint lastEpochTimeDiff = (lastAccumulatedEpoch + 1) *
            EPOCH_DURATION +
            stakingStartTime;
        lastEpochTimeDiff -= prevTimestamp;

        for (uint i = lastAccumulatedEpoch; i <= currentEpoch; i++) {
            EpochInfo storage epoch = epochs[i];
            epochEmissions = (rewardOffset * 28) / 100_00;
            rewardOffset -= epochEmissions;
            // NOTE Previous epoch accumulated
            if (i == lastAccumulatedEpoch) {
                epochEmissions = epochEmissions / EPOCH_DURATION; // PER SECOND

                lastEpochTimeDiff =
                    (epochEmissions * lastEpochTimeDiff * MAGNIFIER) /
                    baseStakingPower;

                accumulatedRewardsPerStakingPower += lastEpochTimeDiff;
                // NOTE Any epochs between last and current epochs
            } else if (i > lastAccumulatedEpoch && i < currentEpoch) {
                baseStakingPower = _latestStakingPower(i);
                accumulatedRewardsPerStakingPower +=
                    (epochEmissions * MAGNIFIER) /
                    baseStakingPower;
                // NOTE current epoch reached
            } else if (i == currentEpoch) {
                epochEmissions = epochEmissions / EPOCH_DURATION; // PER SECOND
                baseStakingPower = _latestStakingPower(i);
                uint diff1 = block.timestamp -
                    (i * EPOCH_DURATION + stakingStartTime);
                diff1 = (epochEmissions * diff1 * MAGNIFIER) / baseStakingPower;
                accumulatedRewardsPerStakingPower += diff1;
            }
            epoch.finalEpochAccumulation = accumulatedRewardsPerStakingPower;
        }
        lastAccumulatedEpoch = currentEpoch;
        lastTotalReward = rewardOffset;
        prevTimestamp = block.timestamp;
    }

    /**
     * Returns the total staking power for the given epoch.
     * @param epochToCheck The epoch to check if adjustement ahs been made
     * @return The total staking power with adjustments made for the given epoch
     */
    function _latestStakingPower(uint epochToCheck) internal returns (uint) {
        EpochInfo storage epoch = epochs[epochToCheck];
        if (epoch.adjusted)
            return totalBaseStakingPower + totalBonusStakingPower;
        epoch.adjusted = true;
        totalBonusStakingPower -= epoch.totalBonusStakingPowerAdjustment;
        return totalBaseStakingPower + totalBonusStakingPower;
    }

    /**
     * Claims the user's current rewards and if time is before lock period, locks them in place. Otherwise it transfers the user their rewards.
     *
     */
    function _claimAndLock(address _user) private {
        _updateAccumulator();

        UserInfo storage user = users[_user];
        if (user.depositAmount == 0) return;
        // Check current EPOCH, if it's <= user.endLockEpoch
        // this determines which stakingPower to use for rewards.
        uint lastEpoch = _getLastEpoch(user.lastAction);
        uint currentEpoch = _currentEpoch();
        uint userStakingPower = user.depositAmount;
        uint accumulatedRewards;
        uint claimableRewards;
        bool hasLocked = user.endLockEpoch != 0;

        if (currentEpoch <= user.endLockEpoch && hasLocked) {
            userStakingPower += user.bonusAmount;
            accumulatedRewards =
                userStakingPower *
                accumulatedRewardsPerStakingPower;
            user.lockedRewards +=
                (accumulatedRewards - user.offsetPoints) /
                MAGNIFIER;
            user.offsetPoints = accumulatedRewards;
            emit LockReward(_user, user.lockedRewards);
        } else if (
            currentEpoch > user.endLockEpoch &&
            lastEpoch <= user.endLockEpoch &&
            hasLocked
        ) {
            // Add full rewards UP TO end of LOCK epoch
            uint endEpochAccumulated = epochs[user.endLockEpoch]
                .finalEpochAccumulation;
            userStakingPower += user.bonusAmount;

            accumulatedRewards = userStakingPower * endEpochAccumulated;
            claimableRewards = accumulatedRewards - user.offsetPoints;

            endEpochAccumulated = user.depositAmount * endEpochAccumulated;

            userStakingPower = user.depositAmount;
            accumulatedRewards =
                userStakingPower *
                accumulatedRewardsPerStakingPower;
            claimableRewards += accumulatedRewards - endEpochAccumulated;
            claimableRewards /= MAGNIFIER;
            claimableRewards += user.lockedRewards;
            user.lockedRewards = 0;
        } else {
            accumulatedRewards =
                userStakingPower *
                accumulatedRewardsPerStakingPower;
            claimableRewards =
                (accumulatedRewards - user.offsetPoints) /
                MAGNIFIER;
            user.offsetPoints = accumulatedRewards;
            user.lockedRewards = 0;
        }

        user.lastAction = block.timestamp;

        if (claimableRewards > 0) {
            WECOIN.transfer(_user, claimableRewards);
            emit ClaimReward(_user, claimableRewards);
        }
    }

    //-----------------------------------------------------------------------------------
    // Internal view functions
    //-----------------------------------------------------------------------------------
    /**
     * @return The current epoch number
     * @dev if staking has not started, it will return 0
     */
    function _currentEpoch() internal view returns (uint256) {
        if (stakingStartTime > block.timestamp) return 0;
        return (block.timestamp - stakingStartTime) / EPOCH_DURATION;
    }

    function _getLastEpoch(uint timestamp) internal view returns (uint) {
        if (stakingStartTime > timestamp) return 0;
        return (timestamp - stakingStartTime) / EPOCH_DURATION;
    }

    //-----------------------------------------------------------------------------------
    // External/Public view/pure functions
    //-----------------------------------------------------------------------------------
    function calculateMultiplier(
        uint _lockedEpochs
    ) public pure returns (uint) {
        return 10000 + (25 * Math.sqrt(_lockedEpochs * SQRT_ADJUSTMENT));
    }

    function getStakingPower(address _user) external view returns (uint) {
        UserInfo storage user = users[_user];
        if (user.endLockEpoch >= _currentEpoch())
            return user.depositAmount + user.bonusAmount;
        return user.depositAmount;
    }

    function getCurrentTotalStakingPower()
        external
        view
        returns (uint totalStakingPower)
    {
        uint currentEpoch = _currentEpoch();
        totalStakingPower = totalBaseStakingPower + totalBonusStakingPower;
        for (uint i = lastAccumulatedEpoch; i <= currentEpoch; i++) {
            EpochInfo storage epoch = epochs[i];
            if (!epoch.adjusted)
                totalStakingPower -= epoch.totalBonusStakingPowerAdjustment;
        }
    }

    function getCurrentEpoch() external view returns (uint) {
        return _currentEpoch();
    }

    function getEpochFromTimestamp(
        uint timestamp
    ) external view returns (uint) {
        return _getLastEpoch(timestamp);
    }

    function pendingRewards(address _user) external view returns (uint) {}
}
