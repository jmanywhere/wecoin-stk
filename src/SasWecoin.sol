// SPDX-License-Identifier: MIT
pragma solidity 0.8.19;

import "openzeppelin/token/ERC20/IERC20.sol";
import "./interfaces/ISasWecoin.sol";
import "openzeppelin/utils/math/Math.sol";

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
    uint private lastAccumulatedEpoch;
    uint private lastTotalReward;
    uint private constant MAGNIFIER = 1e12;
    uint private constant EPOCH_DURATION = 1 weeks;
    uint private constant SQRT_ADJUSTMENT = 100_00;
    uint private constant DAILY_EMISSIONS = 4;
    uint private constant WEEKLY_EMISSIONS = 28;
    uint private constant EMISSIONS_BASE = 100_00;

    constructor(address _wecoin, uint _stakingStartTime) {
        WECOIN = IERC20(_wecoin);
        stakingStartTime = _stakingStartTime;
        prevTimestamp = _stakingStartTime;
    }

    function deposit(uint _amount, uint256 _weeksLocked) external {
        UserInfo storage user = users[msg.sender];
        // TODO Lock or claim rewards
        // NOTE here is where offsetpoints are updated 1 time
        // claimOrLockRewards(msg.sender);
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
        if (_weeksLocked > 0 || user.endLockEpoch >= currentEpoch) {
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

    function claim(address _user) private returns (uint256) {
        UserInfo storage user = users[_user];

        //TODO - we have to know the user staking power duration.
        //TODO - we have to know the user staking power amount.

        _updateAccumulator();
        // TODO need to make correct calculation of rewards;
        return
            (user.depositAmount *
                accumulatedRewardsPerStakingPower -
                user.offsetPoints) / MAGNIFIER;
    }

    function updateAccumulator() external {
        _updateAccumulator();
    }

    function addTokenRewards(uint amount) external {
        WECOIN.transferFrom(msg.sender, address(this), amount);
        totalRewards += amount;
        lastTotalReward += amount;
    }

    //-----------------------------------------------------------------------------------
    // Internal functions
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
        uint totalEmitted = 0;
        uint epochEmissions = 0;
        uint baseStakingPower = _getLatestStakingPower(lastAccumulatedEpoch);
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
                baseStakingPower = _getLatestStakingPower(i);
                accumulatedRewardsPerStakingPower +=
                    (epochEmissions * MAGNIFIER) /
                    baseStakingPower;
                // NOTE current epoch reached
            } else if (i == currentEpoch) {
                epochEmissions = epochEmissions / EPOCH_DURATION; // PER SECOND
                baseStakingPower = _getLatestStakingPower(i);
                uint diff1 = block.timestamp -
                    (i * EPOCH_DURATION + stakingStartTime);
                diff1 = (epochEmissions * diff1 * MAGNIFIER) / baseStakingPower;
                accumulatedRewardsPerStakingPower += diff1;
            }
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
    function _getLatestStakingPower(uint epochToCheck) internal returns (uint) {
        EpochInfo storage epoch = epochs[epochToCheck];
        if (epoch.adjusted)
            return totalBaseStakingPower + totalBonusStakingPower;
        epoch.adjusted = true;
        totalBonusStakingPower -= epoch.totalBonusStakingPowerAdjustment;
        return totalBaseStakingPower + totalBonusStakingPower;
    }

    //-----------------------------------------------------------------------------------
    // Internal view functions
    //-----------------------------------------------------------------------------------
    /**
     * @return The current epoch number
     */
    function _currentEpoch() internal view returns (uint256) {
        return (block.timestamp - stakingStartTime) / EPOCH_DURATION;
    }

    /**
     * This returns the multiplier for the deposited amount. It has 4 decimal place adjustment.
     * @param _lockedEpochs The amount of epochs the user is locked for
     * @return The multiplier based on the amount of epochs locked with 4 decimal places
     */
    function calculateMultiplier(
        uint _lockedEpochs
    ) internal pure returns (uint) {
        return 10000 + (25 * Math.sqrt(_lockedEpochs * SQRT_ADJUSTMENT));
    }

    //-----------------------------------------------------------------------------------
    // External/Public view/pure functions
    //-----------------------------------------------------------------------------------
    function getStakingPower(address _user) external view returns (uint) {
        UserInfo storage user = users[_user];
        if (user.endLockEpoch >= _currentEpoch())
            return user.depositAmount + user.bonusAmount;
        return user.depositAmount;
    }
}
