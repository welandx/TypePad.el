# TypePad.el
Emacs 内的跟打器，目前处于开发中，可以进行简单的测试
## 使用
~~现在的typepad-pyim.el依赖pyim，且不会自动处理依赖~~

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
    - 顶屏的时候会多计一次
    - 暂时解决了形码自动上屏的问题
- [X] diff highlight
- [ ] emacs-rime?
- [X] 击键数据, 码长
  - [X] 计时器
- [X] 结束统计
- [ ] 载文, 发文
  - [x] 载文路径
  - [ ] 自动发文
- [X] 纵向分屏
- [X] 优化 UI
- [ ] 存储数据, 分析报告
- [ ] 随机发文
  - [X] 随机已发内容
  - [ ] 随机全文
- [ ] hook buffer local
- [ ] 调整 hook 顺序
- [ ] 键准数据
- [ ] 速度数据
- [ ] 依赖处理
- [ ] sqlite 储存数据
## 演示
设置了击键目标为4，第一段为击键达标跳转到发文区，调用 `typepad-send-next` 发下一段
第二段打没有达到击键目标，直接在跟打区清空了buffer

https://github.com/welandx/TypePad/assets/59045899/3f8be5e5-deb4-4319-b096-51b7013dfa88

演示中使用的输入法为 [huma-pyim](https://github.com/Neikice/huma_pyim)
