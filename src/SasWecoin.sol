// SPDX-License-Identifier: MIT
pragma solidity 0.8.19;

import "openzeppelin/token/ERC20/IERC20.sol";
import "./interfaces/ISasWecoin.sol";

contract SasWecoin is ISasWecoin {
    mapping(address => UserInfo) public users;

    IERC20 public WECOIN;

    uint public totalRewards;
    uint public stakingStartTime;
    uint public prevTimestamp;
    uint public totalStakingPower;
    uint public accumulatedRewardsPerStakingPower;
    uint private lastAccumulatedEpoch;
    uint private lastTotalReward;
    uint private constant MAGNIFIER = 1e12;

    function claim(address _user) external view returns (uint256) {
        UserInfo storage user = users[_user];

        //TODO - we have to know the user staking power duration.
        //TODO - we have to know the user staking power amount.

        uint lastAction = prevTimestamp;
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
}
