# TypePad.el
Emacs 内的跟打器，目前处于开发中，可以进行简单的测试
## 使用
git clone TypePad 到本地，然后：

```emacs-lisp
(add-to-list 'load-path "/path/to/TypePad")
(require 'typepad)
```

## RoadMap
详见 [Design](Design.org)
- [X] 创建 buffer
- [X] 统计 pyim 的输入次数
  - [X] 测试 huma_pyim 的输入次数
- [X] diff highlight
- [ ] emacs-rime?
- [X] 击键数据, 码长
  - [X] 计时器
- [X] 结束统计
- [ ] 载文, 发文
  - [x] 载文路径
  - [x] 自动发文
  - [x] 列表载文
  - [ ] 继续发文
    - [ ] 非乱序继续
    - [ ] 乱序继续
- [X] 纵向分屏
- [X] 优化 UI
- [x] 存储数据, 分析报告
- [x] 随机发文
  - [X] 随机已发内容
  - [x] 随机全文
- [x] hook buffer local
- [ ] 调整 hook 顺序
- [x] 键准数据
- [x] 速度数据
- [ ] 依赖处理
- [ ] sqlite 储存数据
  - [x] 文章表
  - [x] 记录表
  - [ ] 重排序列
- [ ] 处理错字
- [ ] 暂停功能
  - [ ] after-focus-change-function
  - [ ] buffer-out-hook
- [x] 生成 Hash
- [ ] fix timer
## 演示
设置了击键目标为4，第一段为击键达标跳转到发文区，调用 `typepad-send-next` 发下一段

第二段打没有达到击键目标，直接在跟打区清空了buffer

https://github.com/welandx/TypePad/assets/59045899/3f8be5e5-deb4-4319-b096-51b7013dfa88

演示中使用的输入法为 [huma-pyim](https://github.com/Neikice/huma_pyim)
