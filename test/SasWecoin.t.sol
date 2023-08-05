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
        assertEq(last, block.timestamp);
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
        assertEq(last, block.timestamp);
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
        assertEq(last, block.timestamp);
        assertEq(lock, 16);
        assertEq(endLock, 16);
        assertEq(lockedRewards, 0);
    }
}
