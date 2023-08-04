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
    uint public totalStakingPower;
    uint public accumulatedRewardsPerStakingPower;
    uint private lastAccumulatedEpoch;
    uint private lastTotalReward;
    uint private constant MAGNIFIER = 1e12;
    uint private constant EPOCH_DURATION = 1 weeks;
    uint private constant SQRT_ADJUSTMENT = 100_00;

    constructor(address _wecoin, uint _stakingStartTime) {
        WECOIN = IERC20(_wecoin);
        stakingStartTime = _stakingStartTime;
        prevTimestamp = _stakingStartTime;
    }

    function deposit(uint _amount, uint256 _weeksLocked) external {
        UserInfo storage user = users[msg.sender];
        // TODO get current epoch
        uint currentEpoch = _currentEpoch();
        // TODO Lock or claim rewards
        // TODO Lock duration set
        uint fullAmount = user.depositAmount + user.bonusAmount;
        user.lastAction = block.timestamp;
        // TODO Deposit amount
        user.depositAmount = fullAmount;
        // TODO need to check if there is a previous locking period and extend it.
        if (_weeksLocked > 0) {
            if (user.endLockEpoch >= currentEpoch) {
                user.endLockEpoch += _weeksLocked;
                user.lockDuration = user.endLockEpoch - currentEpoch;
            } else {
                user.endLockEpoch = _weeksLocked + currentEpoch;
                user.lockDuration = _weeksLocked;
            }
            uint multiplier = calculateMultiplier(user.lockDuration);
            user.bonusAmount = (fullAmount * multiplier) / SQRT_ADJUSTMENT;
            totalStakingPower += user.bonusAmount;
        } else {
            totalStakingPower += _amount;
        }
        // Transfer in WECOIN
        WECOIN.transferFrom(msg.sender, address(this), _amount);
    }

    function claim(address _user) external view returns (uint256) {
        UserInfo storage user = users[_user];

        //TODO - we have to know the user staking power duration.
        //TODO - we have to know the user staking power amount.

        uint epochCounter = (prevTimestamp - stakingStartTime) / 7 days;
        uint currentEpoch = (block.timestamp - stakingStartTime) / 7 days;

        uint diff0 = (epochCounter + 1) * 7 days + stakingStartTime;
        diff0 -= user.lastAction;

        uint rewardOffset = lastTotalReward;
        uint totalEmitted = 0;
        uint epochEmissions = 0;
        for (uint i = lastAccumulatedEpoch; i <= currentEpoch; i++) {
            epochEmissions = (rewardOffset * 28) / 100_00;
            rewardOffset -= epochEmissions;
            if (i == epochCounter) {
                epochEmissions = epochEmissions / 7 days; // PER SECOND
                diff0 =
                    (epochEmissions * diff0 * MAGNIFIER) /
                    totalStakingPower;
                accumulatedRewardsPerStakingPower += diff0;
                lastTotalReward = rewardOffset;
            } else if (i > epochCounter && i < currentEpoch) {
                accumulatedRewardsPerStakingPower +=
                    (epochEmissions * MAGNIFIER) /
                    totalStakingPower;
            } else if (i == currentEpoch) {
                uint diff1 = block.timestamp - (i * 7 days + stakingStartTime);
                diff1 =
                    (epochEmissions * diff1 * MAGNIFIER) /
                    totalStakingPower;
                accumulatedRewardsPerStakingPower += diff1;

                lastAccumulatedEpoch = currentEpoch;
                lastTotalReward = rewardOffset;
            }
        }

        return
            (user.depositAmount *
                accumulatedRewardsPerStakingPower -
                user.offsetPoints) / MAGNIFIER;
    }

    //-----------------------------------------------------------------------------------
    // Internal functions
    //-----------------------------------------------------------------------------------
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
}
