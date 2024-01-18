# TypePad.el
Emacs 内的跟打器
## Installation
如果使用的 `use-package` 版本支持 `:vc`
```emacs-lisp
(use-package typepad
  :vc (:url "https://github.com/welandx/TypePad.el"
        :branch master))
```
或使用 `straight`
```emacs-lisp
(straight-use-package '(typepad :type git :host github
                         :repo "welandx/TypePad.el"
                         :files (:defaults "buffer-focus-hook/*.el")))
```
## Usage
1. 设置 `typepad-text-path`, 发文的文本所在目录
2. `M-x typepad-create-sqlite`, 创建数据库
3. `typepad-load-dir`, 加载路径
4. `typepad-create-window`, 创建窗口
5. `typepad-load`, 开始发文
6. 如果要继续上次的发文 `typepad-continue-send`
7. 切换 buffer 后或切出后继续 `typepad-focus-return`
   
   恢复窗口 `typepad-re-window`

### Customization
1. `tp-load-short` 和 `tp-load-long` 提供两种预设
2. `typepad-key-rate-goal` 击键目标

    `typepad-use-key-rate-goal` 是否使用击键目标
3. `typepad-key-acc-goal` 键准目标
4. `typepad-split-size` 分段大小 `tp-set-split` 设置该值
5. `typepad-randomp` 是否乱序全文 `typepad-auto-next` 是否自动下一段

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
- [x] 载文, 发文
  - [x] 载文路径
  - [x] 自动发文
  - [x] 列表载文
  - [x] 继续发文
    - [x] 非乱序继续
    - [x] 乱序继续
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
- [x] sqlite 储存数据
  - [x] 文章表
  - [x] 记录表
  - [x] 重排序列
- [ ] 处理错字
- [x] 暂停功能
  - [x] after-focus-change-function
  - [x] buffer-out-hook
- [x] 生成 Hash
- [x] fix timer
- [x] db path
- [ ] popup transient
## 演示
设置了击键目标为4，第一段为击键达标跳转到发文区，调用 `typepad-send-next` 发下一段

第二段打没有达到击键目标，直接在跟打区清空了buffer

https://github.com/welandx/TypePad/assets/59045899/3f8be5e5-deb4-4319-b096-51b7013dfa88

演示中使用的输入法为 [huma-pyim](https://github.com/Neikice/huma_pyim)
