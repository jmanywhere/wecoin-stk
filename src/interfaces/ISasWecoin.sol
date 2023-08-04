// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

interface ISasWecoin {
    /*-----------------------------------------------------------\
    |                      Type Definitions                      |
    \-----------------------------------------------------------*/
    struct UserInfo {
        uint depositAmount;
        uint bonusAmount;
        uint offsetPoints;
        uint lastAction;
        uint lockDuration; // amount of weeks
        uint endLockEpoch; // Actual END LOCK EPOCH
        uint lockedRewards;
    }
    struct EpochInfo {
        // final epoch accumulation is the accumulatedRewardsStakingPower
        uint finalEpochAccumulation;
        uint totalStakingPowerAdjustment;
    }

    /**
     * Returns the staking power of the user, relative to the user's deposit amount and time staked.
     * @param _user The address of the user
     */
    function getStakingPower(address _user) external view returns (uint256);

    /**
     * Add rewards to the total reward pool.
     * @param _amount The amount of WECOIN to deposit
     */
    function addTokenRewards(uint256 _amount) external;

    /**
     * Deposit WECOIN tokens to create staking power and determine the user's share of the reward pool.
     * @param _amount Amount of WECOIN to deposit
     * @param _weeksLocked Time to LOCK the WECOIN tokens for
     * @dev _weeksLocked must be saved in WEEKS, not seconds
     * @dev penalty pool must be distributed amongst users that are currently staked with a duration
     */
    function deposit(uint256 _amount, uint256 _weeksLocked) external;
}
