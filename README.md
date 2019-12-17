# SimpleMD

## TODO

- Block elements
  - [x] Headers
  - [ ] Blockquotes
  - [ ] Ordered list
  - [ ] Unordered list
  - [x] Code blocks
  - [ ] Horizontal rules
- Inline elements
  - [ ] Links
  - [ ] Emphasis
  - [ ] Code
  - [ ] Automatic links
- Miscellaneous
  - [ ] Backslash escape

## Algorithm

- Loop, check each line
  - Check top of `state_stack`：
    - = `after_pre`
      - If `head_str` = ```` ``` ````
        - End `<pre>`
        - Pop top of `state_stack`
      - Else
        - Continue in `<pre>`
    - = `after_list`
      - If `head_str` = `-` / `x.`
        - Continue in `<list>`
      - If `head_str` = ```` ``` ````
        - End `<list>`
        - Start `<pre>`
      - If `is_blank`
        - End `<list>`
      - Else
        - Continue in `<list item>`
    - = `after_blockquote`
      - If `head_str` = ```` ``` ````
        - End `<blockquote>`
        - Start `<pre>`
      - If `head_str` = `-` / `x.`
        - End `<blockquote>`
        - Start `<list>`
      - Else
        - Continue in `<blockquote>`
    - Else
      - If `head_str` = ```` ``` ````
        - Start `<pre>`
      - If `head_str` = `-` / `x.`
        - Start `<list>`
      - If `head_str` = `>`
        - Start `<blockquote>`
      - Else
        - Start `<p>`
