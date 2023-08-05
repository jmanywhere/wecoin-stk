// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

interface ISasWecoin {
    /*-----------------------------------------------------------\
    |                      Type Definitions                      |
    \-----------------------------------------------------------*/
    struct UserInfo {
        uint depositAmount;
        uint bonusAmount;
        uint offsetPoints; // DEBT to update based on current accumulator
        uint lastAction;
        uint lockDuration; // amount of weeks
        uint endLockEpoch; // Actual END LOCK EPOCH
        uint lockedRewards;
    }
    struct EpochInfo {
        uint epochTotalBaseReward;
        // final epoch accumulation is the accumulatedRewardsStakingPower
        uint finalEpochAccumulation;
        // This adjustment only is added/substracted to the totalBonusStakingPower when the epoch starts.
        uint totalBonusStakingPowerAdjustment;
        bool adjusted;
    }

    /**
     * Returns the staking power of the user, relative to the user's deposit amount and time staked.
     * @param _user The address of the user
     * @return The current staking power of the user
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

    /**
     * Update the reward tracker to the current epoch and timestamp.
     */
    function updateAccumulator() external;

    /**
     * Calculate the multiplier a user would get for a certain amount of epochs.
     * @param _lockedEpochs The amount of epochs to lock tokens for
     * @return The multiplier with 4 decimal places.
     * @dev Minimum is always 1x => 10000
     */
    function calculateMultiplier(
        uint _lockedEpochs
    ) external pure returns (uint);

    /**
     * Returns the total staking power of the contract.
     * @return The total staking power of the contract.
     */
    function getCurrentTotalStakingPower() external view returns (uint);

    //---------------------------------------------------
    //                  EVENTS
    //---------------------------------------------------
    event LockReward(address indexed user, uint256 totalAmountLocked);

    event Deposit(
        address indexed user,
        uint256 amount,
        uint256 bonus,
        uint256 totalLockPeriod
    );

    event Withdraw(address indexed user, uint256 amount);

    event ClaimReward(address indexed user, uint256 amount);
    event WithdrawPenalty(
        address indexed user,
        uint256 principalPenalty,
        uint256 rewardPenalty
    );
}
