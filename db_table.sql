-- phpMyAdmin SQL Dump
-- version 4.0.9
-- http://www.phpmyadmin.net
--
-- 主机: localhost
-- 生成日期: 2013-12-03 09:19:22
-- 服务器版本: 5.1.28-rc-community
-- PHP 版本: 5.2.6

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";

--
-- 数据库: `myserver`
--
CREATE DATABASE IF NOT EXISTS `myserver` DEFAULT CHARACTER SET latin1 COLLATE latin1_swedish_ci;
USE `myserver`;

-- --------------------------------------------------------

--
-- 表的结构 `admin_log`
--

CREATE TABLE `admin_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `admin_name` varchar(20) NOT NULL COMMENT '管理员名称',
  `event` tinyint(2) NOT NULL COMMENT '1登录',
  `ctime` int(10) NOT NULL COMMENT '创建时间',
  `ip` varchar(15) NOT NULL COMMENT 'IP',
  `memo` text NOT NULL,
  PRIMARY KEY (`id`),
  KEY `admin_name` (`admin_name`,`ctime`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COMMENT='后台管理日志';

-- --------------------------------------------------------

--
-- 表的结构 `arena`
--

CREATE TABLE `arena` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增ID',
  `robot` tinyint(1) NOT NULL COMMENT '是否为机器人(1=是,0=否)',
  `rid` int(11) NOT NULL COMMENT '玩家ID',
  `lev` tinyint(2) NOT NULL,
  `exp` int(11) NOT NULL,
  `power` int(11) NOT NULL DEFAULT '0' COMMENT '战斗力',
  `picture` tinyint(1) NOT NULL COMMENT '头像',
  `name` varchar(255) NOT NULL COMMENT '名字',
  `rival` blob NOT NULL COMMENT '对手信息',
  `report` blob NOT NULL COMMENT '战报列表',
  `s` blob NOT NULL COMMENT '英雄',
  PRIMARY KEY (`id`),
  UNIQUE KEY `robot` (`robot`,`rid`),
  KEY `lev` (`lev`),
  KEY `name` (`name`),
  KEY `exp` (`exp`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `attain`
--

CREATE TABLE `attain` (
  `id` int(11) NOT NULL,
  `ctime` int(11) NOT NULL,
  `attain` blob NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `base_admin_user`
--

CREATE TABLE `base_admin_user` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `username` varchar(20) NOT NULL COMMENT '登录名',
  `status` tinyint(1) unsigned NOT NULL COMMENT '激活状态',
  `passwd` varchar(32) NOT NULL COMMENT '密码(md5)',
  `name` varchar(20) NOT NULL COMMENT '真实姓名',
  `description` text NOT NULL COMMENT '描述',
  `last_visit` int(10) unsigned NOT NULL COMMENT '最后登录时间',
  `last_ip` varchar(15) NOT NULL COMMENT '最后登录点IP',
  `last_addr` varchar(30) NOT NULL COMMENT '最后登录地点',
  `login_times` int(10) unsigned NOT NULL COMMENT '登录次数',
  `group_id` smallint(3) NOT NULL COMMENT '所属用户组ID',
  `ip_limit` varchar(150) NOT NULL DEFAULT ' ',
  `error_ip` char(15) NOT NULL DEFAULT '' COMMENT '出错的ip',
  `error_time` int(10) NOT NULL DEFAULT '0' COMMENT '出错时间',
  `error_num` tinyint(2) NOT NULL DEFAULT '0' COMMENT '出错次数',
  `members` varchar(150) NOT NULL DEFAULT ' ' COMMENT '属下成员后台登录名称',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='后台管理员帐号';

-- --------------------------------------------------------

--
-- 表的结构 `base_admin_user_group`
--

CREATE TABLE `base_admin_user_group` (
  `id` smallint(3) NOT NULL AUTO_INCREMENT,
  `name` varchar(20) NOT NULL COMMENT '用户组名称',
  `menu` text NOT NULL COMMENT '菜单权限id,,',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COMMENT='后台用户组';

-- --------------------------------------------------------

--
-- 表的结构 `hero`
--

CREATE TABLE `hero` (
  `role_id` int(11) NOT NULL,
  `hero_id` int(11) NOT NULL,
  `val` blob NOT NULL,
  PRIMARY KEY (`role_id`,`hero_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `item`
--

CREATE TABLE `item` (
  `role_id` int(11) NOT NULL,
  `item_id` int(11) NOT NULL,
  `val` blob NOT NULL,
  PRIMARY KEY (`role_id`,`item_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `log_active_num`
--

CREATE TABLE `log_active_num` (
  `day` int(11) NOT NULL,
  `date` int(11) NOT NULL,
  `num` int(11) NOT NULL,
  PRIMARY KEY (`day`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- 表的结构 `log_buy`
--

CREATE TABLE `log_buy` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `role_id` int(11) NOT NULL COMMENT '角色ID',
  `place` tinyint(1) NOT NULL COMMENT '购买地点：1商城，2房间',
  `tid` int(11) NOT NULL COMMENT '物品类型ID',
  `num` smallint(3) NOT NULL COMMENT '数量',
  `gold` int(11) NOT NULL COMMENT '金币',
  `card` int(11) NOT NULL COMMENT '点券',
  `ctime` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='购买日志';

-- --------------------------------------------------------

--
-- 表的结构 `log_card`
--

CREATE TABLE `log_card` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT 'è‡ªå¢žID',
  `rid` int(11) unsigned NOT NULL COMMENT 'è§’è‰²ID',
  `type` int(11) unsigned NOT NULL COMMENT 'ç­–åˆ’å®šä¹‰çš„ç±»åž‹',
  `num` int(11) NOT NULL COMMENT 'æ•°é‡(è´Ÿæ•°ä¸ºæ¶ˆè€—)',
  `rest` int(11) unsigned NOT NULL COMMENT 'å‰©ä½™ç‚¹åˆ¸',
  PRIMARY KEY (`id`),
  KEY `rid` (`rid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='ç‚¹åˆ¸æµæ°´æ—¥å¿—';

-- --------------------------------------------------------

--
-- 表的结构 `log_combine`
--

CREATE TABLE `log_combine` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `role_id` int(11) NOT NULL,
  `tid` int(11) NOT NULL COMMENT '物品类型ID',
  `ctime` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='合成日志';

-- --------------------------------------------------------

--
-- 表的结构 `log_del_item`
--

CREATE TABLE `log_del_item` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `role_id` int(11) NOT NULL,
  `tid` int(11) NOT NULL COMMENT '物品类型ID',
  `ctime` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `log_diamond`
--

CREATE TABLE `log_diamond` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '自增ID',
  `role_id` int(11) unsigned NOT NULL COMMENT '角色ID',
  `type` int(11) unsigned NOT NULL COMMENT '类型',
  `num` int(11) NOT NULL COMMENT '数量',
  `rest` int(11) unsigned NOT NULL COMMENT '剩余',
  `ctime` int(10) unsigned NOT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`),
  KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COMMENT='钻石流动日志';

-- --------------------------------------------------------

--
-- 表的结构 `log_economy`
--

CREATE TABLE `log_economy` (
  `day_stamp` int(10) unsigned NOT NULL COMMENT '每天0点时间戳',
  `mon_date` int(10) unsigned NOT NULL COMMENT '月份（如201311）',
  `gold_add` int(10) unsigned NOT NULL COMMENT '当天金币总产出',
  `gold_sub` int(10) unsigned NOT NULL COMMENT '当天金币总消耗',
  `diamond_add` int(10) unsigned NOT NULL COMMENT '当天钻石总产出',
  `diamond_sub` int(10) unsigned NOT NULL COMMENT '当天钻石总消耗',
  PRIMARY KEY (`day_stamp`),
  KEY `mon_stamp` (`mon_date`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='经济流动统计日志';

-- --------------------------------------------------------

--
-- 表的结构 `log_enhance`
--

CREATE TABLE `log_enhance` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `role_id` int(11) NOT NULL,
  `tid` int(11) NOT NULL,
  `lev` int(11) NOT NULL,
  `ctime` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='强化日志';

-- --------------------------------------------------------

--
-- 表的结构 `log_gm_send`
--

CREATE TABLE `log_gm_send` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `type` tinyint(1) NOT NULL COMMENT '0=赠送，1=支付',
  `rid` int(11) NOT NULL COMMENT '角色ID',
  `gold` int(11) NOT NULL DEFAULT '0' COMMENT '金币',
  `card` int(11) NOT NULL DEFAULT '0' COMMENT '点券',
  `item_id` int(11) NOT NULL DEFAULT '0' COMMENT '物品ID',
  `num` int(11) NOT NULL DEFAULT '0' COMMENT '物品数量',
  `ctime` int(11) NOT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`),
  KEY `rid` (`rid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `log_gold`
--

CREATE TABLE `log_gold` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '自增ID',
  `role_id` int(11) unsigned NOT NULL COMMENT '角色ID',
  `type` int(11) unsigned NOT NULL COMMENT '类型',
  `num` int(11) NOT NULL COMMENT '数量',
  `rest` int(11) unsigned NOT NULL COMMENT '剩余',
  `ctime` int(10) unsigned NOT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`),
  KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COMMENT='金币流动日志';

-- --------------------------------------------------------

--
-- 表的结构 `log_login`
--

CREATE TABLE `log_login` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '角色id',
  `event` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT '事件：0=登录,1=正常退出,2＝系统关闭时被迫退出,3＝被动退出,4＝其它情况导致的退出',
  `day_stamp` int(11) unsigned NOT NULL DEFAULT '0',
  `login_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '时间',
  `logout_time` int(11) unsigned NOT NULL DEFAULT '0',
  `ip` char(15) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `role_id` (`role_id`),
  KEY `event` (`event`),
  KEY `day_stamp` (`day_stamp`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `log_login_times`
--

CREATE TABLE `log_login_times` (
  `ctime` int(10) NOT NULL COMMENT '创建时间',
  `json` text NOT NULL COMMENT '各个等级人数json',
  PRIMARY KEY (`ctime`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COMMENT='每天登录次数统计表';

-- --------------------------------------------------------

--
-- 表的结构 `log_online_num`
--

CREATE TABLE `log_online_num` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `time_stamp` int(10) NOT NULL DEFAULT '0' COMMENT '统计时间',
  `day_stamp` int(11) NOT NULL COMMENT '日期（天）',
  `num` smallint(5) NOT NULL COMMENT '在线人数',
  PRIMARY KEY (`id`),
  KEY `num` (`num`),
  KEY `day_stamp` (`day_stamp`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COMMENT='在线人数统计';

-- --------------------------------------------------------

--
-- 表的结构 `log_online_time`
--

CREATE TABLE `log_online_time` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `role_id` int(11) NOT NULL COMMENT '角色ID',
  `otime` int(10) NOT NULL COMMENT '当天在线时间累计',
  `ctime` int(10) NOT NULL COMMENT '本记录创建时间',
  PRIMARY KEY (`id`),
  KEY `role_id` (`role_id`),
  KEY `ctime` (`ctime`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `log_online_top`
--

CREATE TABLE `log_online_top` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `ctime` int(10) NOT NULL COMMENT '时间',
  `num` mediumint(6) NOT NULL COMMENT '在线人数',
  PRIMARY KEY (`id`),
  KEY `ctime` (`ctime`,`num`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COMMENT='按日期记录当天最高在线';

-- --------------------------------------------------------

--
-- 表的结构 `log_payment`
--

CREATE TABLE `log_payment` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `created` int(11) NOT NULL,
  `shop_id` int(11) NOT NULL,
  `count` int(11) NOT NULL,
  `pf_money` varchar(200) NOT NULL,
  `pf_uid` varchar(200) NOT NULL,
  `pf_order_id` varchar(200) NOT NULL,
  `msg` varchar(2000) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `pf_order_id` (`pf_order_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `log_reg`
--

CREATE TABLE `log_reg` (
  `id` int(50) NOT NULL COMMENT '角色ID',
  `aid` varchar(255) NOT NULL COMMENT '帐号ID',
  `ctime` int(11) unsigned NOT NULL,
  `day_stamp` int(11) NOT NULL,
  `day_date` int(11) NOT NULL,
  `ip` char(15) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `aid` (`aid`),
  KEY `day_date` (`day_date`),
  KEY `day_stamp` (`day_stamp`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='注册日志';

-- --------------------------------------------------------

--
-- 表的结构 `log_reg_num`
--

CREATE TABLE `log_reg_num` (
  `day` int(11) NOT NULL COMMENT '注册日期（unixstamp）',
  `date` int(11) NOT NULL COMMENT '注册日期（20131122）',
  `num` int(11) NOT NULL COMMENT '注册人数',
  PRIMARY KEY (`day`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='注册人数';

-- --------------------------------------------------------

--
-- 表的结构 `log_retention`
--

CREATE TABLE `log_retention` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `reg_date` int(11) unsigned NOT NULL COMMENT '注册日期',
  `reg_stamp` int(11) unsigned NOT NULL,
  `reg_num` int(11) unsigned NOT NULL,
  `login_num` int(11) unsigned NOT NULL,
  `nth_day` tinyint(3) unsigned NOT NULL COMMENT '第X天留存率',
  `rate` tinyint(3) unsigned NOT NULL COMMENT '留存率',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='留存率日志';

-- --------------------------------------------------------

--
-- 表的结构 `log_slow_call`
--

CREATE TABLE `log_slow_call` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `cmd` int(11) unsigned NOT NULL,
  `data` text NOT NULL,
  `dt` int(11) unsigned NOT NULL,
  `ts` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `log_upgrade`
--

CREATE TABLE `log_upgrade` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(10) NOT NULL COMMENT '角色ID',
  `lev` tinyint(3) unsigned NOT NULL COMMENT '新等级',
  `ctime` int(10) NOT NULL COMMENT '升级时间',
  `msg` varchar(255) DEFAULT NULL COMMENT '升级消息',
  PRIMARY KEY (`id`),
  KEY `role` (`role_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='角色升级日志表';

-- --------------------------------------------------------

--
-- 表的结构 `log_user_stat`
--

CREATE TABLE `log_user_stat` (
  `day_stamp` int(10) unsigned NOT NULL COMMENT '注册日期（unixstamp）',
  `day_date` int(10) unsigned NOT NULL COMMENT '注册日期（20131122）',
  `mon_date` int(10) unsigned NOT NULL,
  `reg_num` int(10) unsigned NOT NULL COMMENT '注册人数',
  `active_num` int(10) unsigned NOT NULL COMMENT '活跃人数',
  `online_num` int(10) unsigned NOT NULL COMMENT '在线人数',
  PRIMARY KEY (`day_stamp`),
  KEY `mon_date` (`mon_date`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='用户每日注册&活跃&登陆统计';

-- --------------------------------------------------------

--
-- 表的结构 `rank_luck`
--

CREATE TABLE `rank_luck` (
  `id` int(11) NOT NULL COMMENT 'ID',
  `name` varchar(50) NOT NULL COMMENT '名字',
  `use_sum` int(11) NOT NULL,
  `val_sum` int(11) NOT NULL,
  `reward_id` int(11) NOT NULL,
  `reward_num` int(11) NOT NULL,
  `ctime` int(11) NOT NULL,
  PRIMARY KEY (`id`,`ctime`),
  KEY `val_sum` (`val_sum`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `role`
--

CREATE TABLE `role` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `gold` int(11) unsigned NOT NULL DEFAULT '0',
  `diamond` int(11) unsigned NOT NULL DEFAULT '0',
  `essence` int(11) unsigned NOT NULL DEFAULT '0',
  `lev` tinyint(3) unsigned NOT NULL,
  `tollgate_id` smallint(5) unsigned NOT NULL DEFAULT '1' COMMENT '关卡ID',
  `aid` varchar(64) NOT NULL,
  `name` varchar(64) NOT NULL,
  `password` varchar(255) DEFAULT NULL,
  `kvs` blob NOT NULL,
  PRIMARY KEY (`id`),
  KEY `lev` (`lev`,`aid`,`name`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COMMENT='角色数据';

-- --------------------------------------------------------

--
-- 表的结构 `test`
--

CREATE TABLE `test` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `bin` varbinary(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `tmp_login`
--

CREATE TABLE `tmp_login` (
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- 表的结构 `tmp_reg`
--

CREATE TABLE `tmp_reg` (
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
