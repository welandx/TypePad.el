# TypePad.el
Emacs 内的跟打器，**只能使用pyim输入**
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
                         :files (:defaults "txt/*")))
```
## Usage
1. 设置 `typepad-text-path`, 发文的文本所在目录
2. `M-x typepad`, 这一步会 load-dir 并创建 db
3. `typepad-load`, 开始发文
4. 如果要继续上次的发文 `typepad-continue-send`
5. 切换 buffer 后或切出后继续 `typepad-focus-return`
6. 发文区 `s` 或跟打区 `M-s` 查看相关的命令
7. 跟打区：`F5` 重打，`M-n` 下一段，`M-p` 上一段
8. 发文区：`n` 下一段，`p` 上一段

### Customization
可设置的选项见 customize group typepad

我的配置：
```emacs-lisp
(use-package typepad
  :hook
  (typepad-mode . visual-fill-column-mode)
  (typepad-readonly-mode . visual-fill-column-mode)
  :config
  (setq typepad-text-path (concat user-emacs-directory "misc/typepad"))
  (add-to-list 'meow-mode-state-list '(typepad-readonly-mode . motion))
  (typepad-load-long))
```

## 演示

https://github.com/welandx/TypePad.el/assets/59045899/41c15706-255d-40f7-b313-7e18bd88976d

演示中使用的输入法为虎码单字，方案文件：[huma_danzi](https://github.com/welandx/huma-danzi.pyim)

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
- [x] 依赖处理
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
- [x] popup transient
- [x] 重打
- [x] keymap
- [x] 默认文本
- [x] 绘表

## 说明
示例文本来自 [木易跟打](https://typer.owenyang.top/portal)

感谢 [pyim](https://github.com/tumashu/pyim) ，只需要 emacs 就可以顺畅输入中文
