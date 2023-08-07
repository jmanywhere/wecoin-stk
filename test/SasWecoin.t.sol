// SPDX-License-Identifier: MIT
pragma solidity 0.8.19;

import "forge-std/Test.sol";
import "../src/SasWecoin.sol";
import "openzeppelin/token/ERC20/presets/ERC20PresetFixedSupply.sol";

contract SasWecoinTest is Test {
    SasWecoin sas;
    ERC20PresetFixedSupply wecoin;

    address user1 = makeAddr("user1");
    address user2 = makeAddr("user2");
    address user3 = makeAddr("user3");
    address user4 = makeAddr("user4");

    uint initStartTime;

    event LockReward(address indexed user, uint256 totalAmountLocked);
    event ClaimReward(address indexed user, uint256 amount);

    function setUp() public {
        wecoin = new ERC20PresetFixedSupply(
            "WECOIN",
            "WECOIN",
            1_000_000 ether,
            address(this)
        );
        initStartTime = block.timestamp + 1 hours;
        sas = new SasWecoin(address(wecoin), block.timestamp + 1 hours);

        wecoin.transfer(user1, 100_000 ether);
        wecoin.transfer(user2, 100_000 ether);
        wecoin.transfer(user3, 100_000 ether);
        wecoin.transfer(user4, 100_000 ether);

        wecoin.approve(address(sas), 1_000_000 ether);
        vm.prank(user1);
        wecoin.approve(address(sas), 100_000 ether);
        vm.prank(user2);
        wecoin.approve(address(sas), 100_000 ether);
        vm.prank(user3);
        wecoin.approve(address(sas), 100_000 ether);
        vm.prank(user4);
        wecoin.approve(address(sas), 100_000 ether);
    }

    function test_add_reward_and_init_state() public {
        //----------------add reward----------------
        sas.addTokenRewards(100_000 ether);
        assertEq(sas.totalRewards(), 100_000 ether);
        assertEq(wecoin.balanceOf(address(sas)), 100_000 ether);

        //-----------------init state----------------
        assertEq(sas.totalBaseStakingPower(), 0);
        assertEq(sas.totalBonusStakingPower(), 0);
        assertEq(sas.accumulatedRewardsPerStakingPower(), 0);
        assertEq(sas.stakingStartTime(), initStartTime);
    }

    modifier addedReward() {
        sas.addTokenRewards(100_000 ether);
        _;
    }

    function test_single_deposit_per_user() public addedReward {
        //------------------deposit with NO lock-------------------------
        vm.prank(user1);
        sas.deposit(100 ether, 0);
        assertEq(wecoin.balanceOf(address(sas)), 100_100 ether);
        uint prevBonus = 0;
        assertEq(sas.totalBaseStakingPower(), 100 ether);
        assertEq(sas.totalBonusStakingPower(), 0);
        assertEq(sas.getCurrentTotalStakingPower(), 100 ether);

        (
            uint deposit,
            uint bonus,
            uint offset,
            uint last,
            uint lock,
            uint endLock,
            uint lockedRewards
        ) = sas.users(user1);
        assertEq(deposit, 100 ether);
        assertEq(bonus, 0);
        assertEq(offset, 0);
        assertEq(last, initStartTime);
        assertEq(lock, 0);
        assertEq(endLock, 0);

        //------------------deposit with  5 week lock--------------------
        vm.prank(user2);
        sas.deposit(100 ether, 5);
        assertEq(wecoin.balanceOf(address(sas)), 100_200 ether);
        uint multiplier = sas.calculateMultiplier(5);
        multiplier = (100 ether * multiplier) / sas.SQRT_ADJUSTMENT();
        prevBonus = multiplier - 100 ether;
        assertEq(sas.totalBaseStakingPower(), 200 ether);
        assertEq(sas.totalBonusStakingPower(), prevBonus);
        assertEq(sas.getCurrentTotalStakingPower(), prevBonus + 200 ether);

        (deposit, bonus, offset, last, lock, endLock, lockedRewards) = sas
            .users(user2);
        assertEq(deposit, 100 ether);
        assertEq(bonus, prevBonus);
        assertEq(offset, 0);
        assertEq(last, initStartTime);
        assertEq(lock, 5);
        assertEq(endLock, 5);
        assertEq(lockedRewards, 0);
        //------------------deposit with 16 week lock--------------------
        vm.prank(user3);
        sas.deposit(100 ether, 16);
        assertEq(wecoin.balanceOf(address(sas)), 100_300 ether);
        multiplier = sas.calculateMultiplier(16);
        multiplier = (100 ether * multiplier) / sas.SQRT_ADJUSTMENT();
        multiplier -= 100 ether;
        prevBonus += multiplier;
        assertEq(sas.totalBaseStakingPower(), 300 ether);
        assertEq(sas.totalBonusStakingPower(), prevBonus);
        assertEq(sas.getCurrentTotalStakingPower(), prevBonus + 300 ether);

        (deposit, bonus, offset, last, lock, endLock, lockedRewards) = sas
            .users(user3);
        assertEq(deposit, 100 ether);
        assertEq(bonus, multiplier);
        assertEq(offset, 0);
        assertEq(last, initStartTime);
        assertEq(lock, 16);
        assertEq(endLock, 16);
        assertEq(lockedRewards, 0);
    }

    function test_single_user_claims_NOLOCK() public addedReward {
        // --------------- NO LOCK ----------------
        vm.prank(user1);
        sas.deposit(100 ether, 0);

        vm.warp(initStartTime + 1 hours); // 1 hour after rewards started accruing

        uint expectedReward = (100_000 ether * 4) / 100_00;
        expectedReward = (expectedReward * 1 hours) / 1 days;

        uint initBalance = wecoin.balanceOf(user1);

        vm.prank(user1);
        // FORGE IS NOT WORKING WITH EXPECT EMITS, WILL HAVE TO MANUALLY CHECK EMISSIONS
        // vm.expectEmit();
        // emit ClaimReward(user1, expectedReward);
        sas.claimOrLock();

        (, , uint offset, uint last, , , uint locked) = sas.users(user1);
        assertGt(offset, 0);
        assertEq(last, block.timestamp);
        assertEq(locked, 0);
        assertEq(wecoin.balanceOf(user1), initBalance + expectedReward);
    }

    function test_single_user_claims_LOCK_16WEEKS() public addedReward {
        // --------------- NO LOCK ----------------
        vm.prank(user1);
        sas.deposit(200 ether, 0);
        // --------------- LOCKED ----------------
        vm.prank(user2);
        sas.deposit(100 ether, 16);

        assertEq(sas.getStakingPower(user2), 200 ether);

        vm.warp(initStartTime + 1 hours); // 1 hour after rewards started accruing

        uint expectedReward = (100_000 ether * 4) / 100_00;
        expectedReward = (expectedReward * 1 hours) / 1 days;
        expectedReward /= 2;

        vm.prank(user2);
        // FORGE IS NOT WORKING WITH EXPECT EMITS, WILL HAVE TO MANUALLY CHECK EMISSIONS
        // vm.expectEmit();
        // emit LockReward(user1, expectedReward);
        sas.claimOrLock();

        (, , uint offset, uint last, , , uint locked) = sas.users(user2);
        assertGt(offset, 0);
        assertEq(last, block.timestamp);
        if (locked > expectedReward) {
            locked -= expectedReward;
            assertLt(locked, 10);
        } else {
            locked = expectedReward - locked;
            assertLt(locked, 10);
        }

        //--------------- LOCKED to UNLOCKED ----------------
        vm.warp(initStartTime + 16 weeks + 6 days + 23 hours + 59 minutes + 59); // wrap up the 16th week
        vm.prank(user2);
        sas.claimOrLock();
        (, , offset, last, , , locked) = sas.users(user2);
        assertGt(locked, 0);

        skip(1 hours + 1);
        assertEq(sas.getCurrentEpoch(), 17);

        vm.prank(user2);
        sas.claimOrLock();

        (, , offset, last, , , locked) = sas.users(user2);
        assertGt(offset, 0);
        assertEq(last, block.timestamp);
        assertEq(locked, 0); // everything was claimed
        assertEq(sas.getStakingPower(user2), 100 ether);
        // --------------- CHECK NO LOCK ----------------
        vm.prank(user1);
        sas.claimOrLock();
    }

    function test_pending_Rewards() public addedReward {
        // --------------- SAME DAY ----------------
        vm.prank(user1);
        sas.deposit(100 ether, 4); // 4 weeks = 1.5 Multiplier
        vm.warp(initStartTime + 1 hours); // 1 hour after rewards started accruing
        uint totalRewardPool = 100_000 ether;
        uint baseEmission = totalRewardPool * 4;
        uint expectedReward = (baseEmission * 1 hours) / uint(100_00 * 1 days);
        assertEq(sas.pendingRewards(user1), expectedReward);

        // ---------------- MULTIPLE EPOCHS ---------
        vm.warp(initStartTime + 2 weeks);
        expectedReward = (baseEmission * 7) / 100_00;
        totalRewardPool -= expectedReward;
        expectedReward += (totalRewardPool * 28) / 100_00;
        uint u1rew = expectedReward;
        assertEq(sas.pendingRewards(user1), expectedReward);
        // ---------------- MULTIPLE USERS -----------
        vm.prank(user2);
        sas.deposit(150 ether, 0); // no duration staking, so equal distribution
        skip(1 weeks);
        totalRewardPool -= expectedReward;

        assertEq(sas.pendingRewards(user2), 139.2170976 ether);
        u1rew += 139.2170976 ether;
        uint u1pending = sas.pendingRewards(user1);
        if (u1rew > u1pending) assertLt(u1rew - u1pending, 10);
        else assertLt(u1pending - u1rew, 10);

        // ---------------- MULTIPLE USERS & 1 EARLY WITHDRAW -------------
    }

    function test_withdraw_before_lock_ends() public addedReward {
        vm.prank(user1);
        sas.deposit(100 ether, 4); // 4 weeks = 1.5 Multiplier
        vm.prank(user2);
        sas.deposit(100 ether, 4); // 4 weeks = 1.5 Multiplier
        vm.prank(user3);
        sas.deposit(100 ether, 4); // 4 weeks = 1.5 Multiplier

        vm.warp(initStartTime + 1 hours); // 1 hour after rewards started accruing

        uint initBalance = wecoin.balanceOf(user1);
        // since all 3 test users entered with the exact same staking power each, each user only gets 1/3 of the toralRewards;
        uint expectedReward = uint(100_000 ether * 4 * 1 hours) /
            uint(100_00 * 1 days * 3);

        vm.prank(user1);
        sas.withdraw();

        assertEq(sas.pendingRewards(user1), 0);
        assertEq(sas.pendingRewards(user2), expectedReward);
        assertEq(sas.pendingRewards(user3), expectedReward);

        assertEq(wecoin.balanceOf(user1), initBalance + 95 ether);
        assertEq(sas.nextEpochRewardAddition(), 5 ether + expectedReward);
        (, , uint adjustment, ) = sas.epochs(5);
        // Magnifier is 0.5x so 100 * 0.5 * 2 = 100 ether
        assertEq(adjustment, 100 ether);

        initBalance = wecoin.balanceOf(user2);
        vm.warp(initStartTime + 3.5 weeks);
        uint fullRewards = sas.pendingRewards(user2);
        vm.prank(user2);
        sas.withdraw();

        assertGt(wecoin.balanceOf(user2), initBalance + 100 ether);
        fullRewards = (fullRewards * 8) / 10; // 80% of the rewards are sent back to pool
        initBalance = sas.nextEpochRewardAddition();
        if (fullRewards > initBalance)
            assertLt(fullRewards - initBalance, 0.1 ether);
        else assertLt(initBalance - fullRewards, 0.1 ether);
    }

    function test_withdraw_after_lock_ends() public addedReward {
        vm.prank(user1);
        sas.deposit(100 ether, 4); // 4 weeks = 1.5 Multiplier
        vm.prank(user2);
        sas.deposit(100 ether, 4); // 4 weeks = 1.5 Multiplier
        vm.prank(user3);
        sas.deposit(100 ether, 4); // 4 weeks = 1.5 Multiplier

        vm.warp(initStartTime + 5 weeks);

        vm.prank(user3);
        sas.withdraw();

        assertEq(sas.nextEpochRewardAddition(), 0);
        assertEq(sas.pendingRewards(user3), 0);
    }
}
