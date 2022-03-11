# 6 类继承

CLOS使用类优先列表来确定任何竞争特性的优先级或优势。本章描述了类优先列表是如何确定的，以及它是如何控制槽和槽选项的继承的。我们还讨论了隐式包含在超类中的类:`standard-object`和`t`。

## 6.1 从默认类继承

所有类都隐式地将`t`包含为超类。对于用户定义的类(使用`defclass`定义的类)和内置类(如数组和整数)，这是正确的。唯一的例外是类`t`本身，它没有超类。从`t`继承的一个效果是，每个Lisp对象都是`t`类型。类型`t`是COMMON LISP 类型系统的根，而`t`类是CLOS类系统的根。

所有用户定义的类也隐式地包括`standard-object`作为超类，但内置类不这样做。`standard-object`的存在使CLOS实现能够定义由所有用户定义类继承的默认行为。例如，`standard-object`类的 primary 方法实现了`print-object`和`describe`泛型函数。`t`和`standard-object`类没有槽。

所有类都将`t`作为其类优先级列表中的最后一个(最泛化的)类。所有用户定义的类都将`standard-object`作为其类优先级列表的倒数第二个类。你可以认为这是理所当然的;这总是正确的。

## 6.2 类优先级列表

CLOS计算每个类的类优先列表。类的优先级列表包含类本身及其所有超类;它不包含任何重复的类。类优先级列表中类的顺序是重要的;从最具体到最不具体。如果一个类比第二个类更匹配，那么这个类优先于第二个类。

管理类的优先顺序的两条规则是:

> **类优先级 规则1**
> 类的优先级总是高于它的超类。
> **类优先级 规则2**
> 每个类定义设置其直接超类的优先顺序。

规则1允许类重写或修改其超类提供的行为; 

对于规则2，直接超类的顺序约束是通过`defclass`表达式中列出的超类的顺序获得的。也就是说，列表中排在前面的类比排在后面的类更匹配。

通过单独考虑规则1，我们知道在任何类优先列表中最具体的类和最不具体的类。类本身在它自己的类优先列表中总是最特定的类，而类`t`在任何类优先列表中都是最泛化的类。(因为每个类都有`t`作为超类，所以`t`不能在任何类之前，因此在所有类的优先级列表中总是最后一个。)对于用户定义的类，`standard-object`是类优先级列表中倒数第二个类。在下面的例子中，我们没有明确地提到`standard-object`或`t`的顺序约束，因为这些约束总是相同的。

当CLOS确定类的类优先列表时，它首先从类的定义开始。CLOS将这两个规则应用于类定义，并获得一组局部排序约束。然后，CLOS将这些规则应用于每个直接超类的定义，以及它们的每个直接超类，等等，直到所有路径都以根类`t`结束。结果是一组对类的排序约束。

下一步是找到满足所有排序约束的总排序。CLOS通过对排序约束集合进行拓扑排序来实现这一点。换句话说，每个约束都是偏序的，类优先列表是通过对偏序集合进行拓扑排序来实现的。结果是以下三种可能性之一:

情况1. 正好有一个总排序满足约束条件；
情况2. 几个总排序满足约束条件；
情况3. 没有总排序满足约束条件

在前两种情况中，CLOS都会生成类优先列表。在第三种情况下，它会抛出一个错误。

我们将针对每一种情况给出例子。这些例子展示了各种流类的定义，这些流类有超类但没有槽。我们不打算描述这些类的语义，而是关注类优先列表的机制。(在[开发一个高级CLOS程序:流]()，第171页，我们开发了一个基于大量类的组织的流示例。)

### 情况1:恰好有一个总排序满足约束条件

在本例中，当CLOS将两个类优先规则应用于类及其所有超类的类定义时，结果是只有一种可能的排序。这将成为类优先级列表。

下面是一个例子。我们的目标是确定类`char-input-stream`的类优先列表，给出以下类定义:

```lisp
(defclass stream () ())
(defclass input-stream (stream) ())
(defclass char-stream (stream) ())

(defclass char-input-stream
  (char-stream input-stream)
  ())
```

下表给出了字符输入流的排序约束集合。符号 ">>" 是“前面”的缩写。每个约束都是将一个类优先规则应用到一个类定义的结果。因此，图表中的第一个条目意味着“类`input-stream`在类`stream`之前，这是对`input-stream`类定义应用规则1的结果。”类似地，最后一个条目意味着“类`char-stream`在类`input-stream`之前，这是将规则2应用于`char-input-stream`类定义的结果。”

| Constraint                            | Rule | Class            |
| ------------------------------------- | --- | ----------------- |
| `input-stream` >> `stream`            | 1 | `input-stream`      |
| `char-stream` >> `stream`             | 1 | `char-stream`       |
| `char-input-stream` >> `char-stream`  | 1 | `char-input-stream` |
| `char-input-stream` >> `input-stream` | 1 | `char-input-stream` |
| `char-stream` >> `input-stream`       | 2 | `char-input-stream` |

只有一个总排序满足约束，所以`char-input-stream`类的类优先列表是

    (char-input-stream char-stream input-stream stream standard-object t)
    
虽然`stream`类包含在两个不同的类中(它是`char-stream`和`input-stream`的直接超类)，但在类优先级列表中没有重复的类。

### 情况2：几个总排序满足约束

对于许多程序，两个类优先级规则不会产生单一的类优先级顺序。也就是说，某些成对的类可能没有基于规则的排序约束。当两个类都不是另一个类的超类时（规则 1 不限制它们的相对优先级），并且没有一个类将这两个类都包含为直接超类（规则 2 不限制它们的相对优先级），就会发生这种情况。这不会造成问题，原因有以下三个： 

- 缺乏约束意味着没有冲突。当两个类没有给出排序约束时，这意味着它们的相对优先顺序不重要。如果两个类的顺序很重要，程序员可以并且应该通过显式地将它们包含为新类的直接超类来设置一个顺序约束。

-CLOS选择一个可能的排序。CLOS使用的算法总是产生确定性的类排名。这保证了在给定相同的类定义集的情况下，CLOS的所有实现都选择相同的类优先列表。算法的细节并不重要，但是保证算法的确定性，为工作的CLOS程序依赖于某种顺序而不需要在类定义中显式声明依赖关系的情况提供了一个安全网。这样的程序可以移植到另一个CLOS实现。

-CLOS试图将家族树保持在类优先列表中。可以将一个类及其超类看作一个“家族树”。当应用算法从一组可能的排序中选择一个排序时，CLOS使用以下准则:如果不违反两个类优先规则中的任何一个，每个直接超类的整个“家族树”都保存在类优先列表中。

假设有一个`ascii-disk-stream`类，它有两个直接超类:`ascii-stream`和`disk-stream`。类`ascii-stream`在`disk-stream`之前，并且(如果可能的话)在类优先级列表中`ascii-stream`的所有超类都在`disk-stream`之前。其效果是，你可以将`ascii-stream`视为行为的“黑盒”;`disk-stream`不能覆盖`ascii-stream`或其任何超类提供的行为。在其他排序约束阻止CLOS遵循这一原则的情况下(也就是说，结果将违反其中一条或两条规则)，CLOS将选择一种使每个家族树的成员尽可能靠近的排序。

这里我们给出了几个总排序满足约束条件的例子。我们确定了类`ascii-disk-stream`的类优先列表，给出了以下类定义:

```lisp
(defclass stream () ())

(defclass buffered-stream (stream) ())
(defclass disk-stream (buffered-stream) ())

(defclass char-stream (stream) ())
(defclass ascii-stream (char-stream) ())

(defclass ascii-disk-stream
  (ascii-stream disk-stream)
  ())
```

`ascii-disk-stream`的排序约束集合如下:

| Constraint                            | Rule | Class             |
| ------------------------------------- | --- | ------------------ |
| `buffered-stream` >> `stream`         | 1 | `buffered-stream`    |
| `disk-stream` >> `buffered-stream`    | 1 | `disk-stream`        |
| `char-stream` >> `stream`             | 1 | `char-stream`        |
| `ascii-stream` >> `char-stream`       | 1 | `ascii-stream`       |
| `ascii-disk-stream` >> `ascii-stream` | 1 | `ascii-disk-stream`  |
| `ascii-disk-stream` >> `disk-stream`  | 1 | `ascii-disk-stream`  |
| `ascii-stream` >> `disk-stream`       | 2 | ` ascii-disk-stream` |

`char-stream`相对于`buffered-stream`的优先级没有限制，`char-stream`相对于`disk-stream`的优先级也没有限制。这里，我们给出了三个满足约束条件的总排序。每个类优先级列表的中间一行显示了变化发生的位置:

```lisp
(ascii-disk-stream ascii-stream char-stream
 disk-stream buffered-stream stream
 standard-object t)
 
(ascii-disk-stream ascii-stream disk-stream
 buffered-stream char-stream stream
 standard-object t)
 
(ascii-disk-stream ascii-stream disk-stream
 char-stream buffered-stream stream
 standard-object t)
```

在本例中，CLOS选择第一个总排序。在这里，我们看到一个指导方针的说明，家族树保持在一起。`ascii-stream`的家族树在`disk-stream`的家族树之前，但`stream`类除外，它是`ascii-stream`和`disk-stream`的超类。

有时不可能保持家族树完整，但是如果两个超类有一个“共同的尾巴”，那么它就会被移到类优先级列表的末尾。假设类A有直接的超类B和C, B和C的类优先级列表如下:

    (A B B1 B2 B3 B4 C C1 C2 D D1 D2 standard-object t)
    
如果几个排序满足约束，则无需担心，除非，事实上，程序依赖于其中一个排序。如果是这样，则应该显式设置排序依赖关系，如下所示。

### 如何添加排序约束

继续前面的例子，假设程序的某些方面依赖于类 `char-stream` 之前的类 `disk-stream`，以及在 `buffered-stream` 之前的类 `char-stream`。也就是说，您希望选择可能的总排序中的三分之一。

在这个例子中，很难想象为什么流程序应该有提到的依赖关系，因为在顺序不受约束的类之间不应该有交互。然而，在其他程序中，各种类之间可能存在交互。

你可以通过另一种方式定义`ascii-disk-stream`类来添加约束:

```lisp
(defclass ascii-disk-stream
  (ascii-stream disk-stream char-stream buffered-stream)
  ())
```

前面提到的约束仍然有效，现在有两个新的约束:

| Constraint                         | Rule | Class             |
| ---------------------------------- | --- | ------------------ |
| `disk-stream` >> `char-stream`     | 2 | `ascii-disk-stream`  |
| `char-stream` >> `buffered-stream` | 2 | `ascii-disk-stream`  |

这些额外的约束只会产生一种可能的总排序:

    (ascii-disk-stream ascii-stream disk-stream
     char-stream buffered-stream stream
     standard-object t)
     
### 情况3：没有总排序满足约束

当一个类被多个类定义包含并且类定义设置的局部约束相互直接冲突时，没有总排序满足约束。

CLOS不能解决这样的冲突，因此它表示错误。然后，您可以编辑类定义，以删除一些冲突的排序约束。下面是一个类组织的例子，其中不可能有完全的顺序。我们尝试确定类`ascii-disk-stream`的类优先列表，给出以下类定义:

```lisp
(defclass stream () ())

(defclass buffered-stream (stream) ())
(defclass disk-stream (buffered-stream) ())

(defclass char-stream (stream) ())
(defclass ascii-stream (char-stream) ())

(defclass ascii-disk-stream
  (ascii-stream buffered-stream disk-stream)
  ())
```

两个类定义会导致冲突。这里我们只展示了相互冲突的约束条件:

| Constraint | Rule | Class |
| ---------------------------------- | --- | ----------------- |
| `disk-stream >> buffered-stream`   | 1 | `disk-stream`       |
| `buffered-stream` >> `disk-stream` | 2 | `ascii-disk-stream` |

在这种情况下，CLOS会发出错误信号，因为它无法生成与排序约束一致的类优先列表。

很明显，这种等级组织是有缺陷的。类`ascii-disk-stream`依赖于`disk-stream`之前的`buffered-stream`，但类`disk-stream`依赖于`buffered-stream`之前的`disk-stream`。

这个特殊的问题可能是由于对类组织的误解造成的。`disk-stream`定义设置的约束似乎是`disk-stream`正确工作所必需的语义约束，但 `ascii-disk-stream` 设置的约束只是程序员错误。可能没有必要将 `buffered-stream` 包含为 `ascii-disk-stream` 的直接超类。

但是，如果类 `ascii-disk-stream` 确实依赖于 `disk-stream` 之前的 `buffered-stream`，那么问题出在类组织的某个地方。解决方案是重新考虑类组织的语义。

### 相反的约束是可能的

可以定义两个包含相反排序约束的类，只要您不尝试定义一个建立在它们之上的类。

```lisp
(defclass stream () ())
(defclass input-stream (stream) ())
(defclass buffered-stream (stream) ())

(defclass disk-stream (buffered-stream input-stream) ())

(defclass tape-stream (input-stream buffered-stream) ())
```

请注意，`disk-stream` 类要求 `buffered-stream` 在 `input-stream` 之前，但 `tape-stream` 类需要 `input-stream` 在 `buffered-stream` 之前。

这些类定义并不冲突，因为类 ``disk-stream` 和 `tape-stream` 之间还没有连接。但是，如果您尝试定义一个建立在 `disk-stream` 和 `tape-stream` 上的类，CLOS会发出错误信号:

    (defclass disk-emulating-tape-stream (disk-stream tape-stream) ())
    

## 6.3 类的组织设计指南

本节讨论类优先规则如何影响编程实践。两个类优先规则的一个重要方面是程序员通过决定要包含哪些直接超类以及它们的顺序应该是什么来控制本地的排序约束。如果所有本地排序约束都是正确的，则生成的类优先级列表将是适当的。在设计类组织时，您应该关注两个规则对每个类定义的影响，而不关心最终的类优先级列表。

> **类优先级 规则1**
> 类的优先级总是高于它的超类。

规则1 建议您应该定义一个或多个基本类并在它们之上构建更专业的类。这种编程风格允许专门的类继承期望的行为并覆盖不需要的行为。

> **类优先级 规则2**
> 每个类定义设置其直接超类的优先顺序。

规则 2 对建立在多个直接超类上的类有影响。在某些情况下，每个直接超类都有不同的贡献，它们之间没有冲突； 那么在超类列表中如何排序它们并不重要。然而，在其他情况下，两个超类提供了相互竞争的特征。例如，它们都可能具有相同泛型函数的primary 方法。在这种情况下，您应该决定两个primary 方法中的哪一个更适合新类，并相应地对两个直接超类进行排序。

规则 2 还鼓励使用 mixin (混入)类的编程风格。在这种风格中，每个 mixin 类都支持单独的、明确定义的行为方面。mixin 的目标是完全支持该行为并且不与其他类发生冲突。例如，mixin 可能提供修改其他类提供的primary 方法的行为的 before 方法和 after 方法。当 mixin 不与其他类竞争时，它的优先顺序并不重要。通常，mixin 将根类作为其唯一的超类，因此它的排序约束是最小的。这允许从许多混入类构建出一个类。

请注意，最终的类优先级列表始终满足这两个规则——并且在大多数情况下，它还遵循将不相交的族谱保持在一起的准则。在大多数情况下，程序可以将每个直接超类视为一个黑盒，并且可以依赖于第二个直接超类及其所有超类之前的第一个直接超类的所有超类。在CLOS不能遵循准则的情况下（由于其他排序约束），生成的类优先级列表允许第二个直接超类的超类在第一个直接超类的超类之前。

如前所述，如果一个程序依赖于一个类比另一个类更匹配，你应该明确排序约束。

## 6.4 槽和槽选项的继承

类可以通过提供槽说明符来定义槽，该说明符包括槽的名字和可能的一些槽选项。除了类在其`defclass`表达式中本地定义的槽外，类还继承由其超类定义的槽和槽选项。

### 修改继承的槽

通过为具有相同名字的槽提供一个本地槽说明符，类可以修改或重写槽的其他方面，否则这些方面将被继承。例如:

```lisp
(defclass basic-lock ()
  ((name :initarg :name)))

(defclass simple-lock (basic-lock)
  ((name :initform "Simple Lock")))
```

类`basic-lock`为名为`name`的槽和`:initarg :name`槽选项提供了槽说明符。

在`basic-lock`基础上构建的`simple-lock`类继承了`name` 槽和`:initarg :name` 槽选项。它还提供了一个本地槽说明符——用于 `name` 槽和 `:initform` 槽选项的说明符。这不会覆盖任何继承的特征，但它增加了一个默认的初始值形式的槽。对于类来说，为继承自超类的槽提供默认的初始值形式通常很有用。

`simple-lock` 的每个实例只有一个名字为`name`的槽。该槽的特征来自类优先级列表中为名字提供槽说明符的所有类。`simple-lock` 的实例从这些类中接收到以下槽特征:

| Slot Characterisics       | From Class    |
| ------------------------- | ------------- |
| `name`槽自身               | `basic-lock`  |
| `:initarg :name`          | `basic-lock`  |
| `:initform "Simple Lock"` | `simple-lock` |

### 每个槽选项的继承行为

这里我们将描述如何继承每个槽选项。槽选项具有不同的继承行为。重要的是要注意，每个槽选项是独立于其他槽选项继承的。

类优先级列表中的每个类都可以通过为具有该名字的槽提供槽说明符来影响槽的特征。CLOS将槽说明符聚集在一起，并根据类优先级列表，从最特定到最泛化对它们进行排序。换句话说，槽指示符的优先级由提供它的类的优先级控制。

这些规则确定槽的最终特征集:

- `:accessor`, `:reader`, `:writer` : 不继承

这些槽选项创建方法，但是不影响槽本身。尽管这些槽选项本身不是继承的，但访问器方法继承的方式与继承任何其他方法的方式相同。

- `:allocation` ： Inherited by shadowing

槽的分配由为该槽提供槽说明符的最具体的类控制，无论是否提供了 `:allocation` 槽选项。

如果最具体的槽说明符提供 `:allocation :instance` 或根本不提供 `:allocation` 槽选项，则此槽是本地槽。如果最具体的槽说明符提供 `:allocation :class`，则这是一个共享槽。（在这种情况下，为这个类创建了一个新的类槽，它的所有实例和任何不提供或继承该槽的更匹配槽说明符的任何子类的实例都可以访问它。）

- `:documentation`: Inherited by shadowing

槽的文档由最特定的槽说明符控制，该说明符为该槽提供`:documentation`槽选项。任何提供`:documentation`槽选项的不太具体的槽说明符将被忽略。

- `:initarg` : Inherited by union

一个槽可以有多个 initarg。如果类优先列表中的几个类为同一个槽提供了`:initarg`槽选项，那么可以使用任意的initargs来初始化槽。

- `:initform` : Inherited by shadowing

槽的初始化由最特定的槽说明符控制，该说明符为该槽提供`:initform`槽选项。任何提供`:initform`的不太具体的槽指示符都会被忽略。

- `:type` : Inherited by "and"

槽的类型由所有提供`:type`槽选项的槽说明符控制。槽的值必须满足所提供的所有类型约束。例如，如果类优先列表中的三个类将类型指定为数字、有理数和整数，那么槽的值必须满足

    (typep value '(and number rational integer))
    
这意味着类不能放松槽上任何继承的类型约束，但它可以使类型约束更严格。

## 6.5 使用槽选项继承的指导原则

槽和槽选项的继承行为可能听起来很复杂。每个槽选项由不同的规则独立继承。每个槽选项的继承行为提供了一个在某些上下文中可能有用的特性，但大多数程序并不需要所有这些特性。

几乎所有的CLOS程序都利用了槽是继承的这一事实。基本类提供了少量的槽，这些槽适用于在其上构建的所有类，而更专门化的类可以包含——包括额外的槽。

许多CLOS程序利用了`:initform`槽选项的继承。在某些情况下，从超类继承默认的初始值是合适的。在其他情况下，重写继承的默认初始值也是有用的。

许多CLOS程序还利用了`:initarg`槽选项的继承。通常，提供槽的类也会提供initarg(如果要初始化槽的话)。有时候，子类提供`:initarg`槽选项来为槽提供另一个initarg是有用的。

当使用`:type`时，通常提供槽的类也指定槽的类型。子类通常继承该类型而不需要进一步约束它。

通常，子类不会选择覆盖槽的分配。类将槽的分配从`:class`更改为`:instance`，或者反之亦然，这是不常见的，因为共享槽与本地槽的语义非常不同。最常见的例子覆盖分配发生在一个类指定一个:类槽,和它的子类选择不共享特定的位置,而是要创建一个新的`:class`槽子类的实例之间共享(及其子类的实例,除非他们也创建一个新的`:class`槽)。
