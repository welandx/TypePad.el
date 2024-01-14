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
    - 顶屏的时候会多计一次
    - 暂时解决了形码自动上屏的问题
- [X] diff highlight
- [ ] emacs-rime?
- [X] 击键数据, 码长
  - [X] 计时器
- [X] 结束统计
- [ ] 载文, 发文
- [X] 纵向分屏
- [X] 优化 UI
- [ ] 存储数据, 分析报告
- [-] 随机发文
  - [X] 随机已发内容
  - [ ] 随机全文
- [ ] hook buffer local
- [ ] 调整 hook 顺序
- [ ] 键准数据

## 演示