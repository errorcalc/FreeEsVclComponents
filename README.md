![Logo](Source/icons/Logo.png)
# FreeEsVCLComponents
Free components for delphi (VCL)
Attribution-ShareAlike 3.0

*** You can help, if you send me where you are using the component and attach a screenshot example of use ***

*** Вы можете помочь, если вы напишете мне, где вы используете компонент и приложите скриншот с примером использования ***

# List of components:
* TEsLayout (Absolitly no flicker! Also for children TGraphicControl`s - BufferedChildrens property)
* TEsImageLayout (Best for your application)
* TEsLabelLayout
* TEsNinePatchImage
* TEsImageLabel
* TEsActivityBar (looks like Windows 10 XAML control!)
* TEsSwitch (looks like Windows 10 XAML control!)

# XE2, XE3, XE4, XE5, XE6, XE7, XE8, RX10(Seattle) - compatibility

#English ReadMe:

** FreeEsVCLComponents ** - a free library of VCL components for Delphi. All components has best support transparency, FreeEsVCLComponents not flicker, support Interesting possibility for double buffering for TGraphicControl heirs - BufferedChildens property.

Pluses:
* Windows XP support
* Support for built-in VCL skins
* Display convenient borders in DesignTime, activate properties IsDrawHelper
* Excellent support for transparency!
* Ability to make transparent to the mouse, activate properties IsTransparentMouse

**TEsCustomControl** - The base class contains the anti-flicker magic and support transparency.

**TEsNinePatchImage** - TImage for 9-Patch images (raster image with the ability to zoom without loss of quality, almost all modern interfaces are built on them), as well as images - Overley

**TEsImageLabel** - The Label supports 9-Patch background image, and image - Overley, the location and alignment of the text can be combined in different ways

**TEsLayout** - The basic container. Interesting possibility for double buffering TGraphicControl heirs (TLabel, TGlyphButton, TImage, TBevel ...)

**TEsImageLayot** - The Layout with support background 9-Patch images and image - Overley

**TEsLabelLayout** - The Layout supports 9-Patch background image, and image - Overley, the location and alignment of the text can be combined in different ways

**TEsActivityBar** - Component simulates XAML control Windows 10, designed to indicate or display the progress of employment (something TProgressBar type). It has a lot of settings.
* Support for two display modes
* Three types of animation and progress indication
* Support VCL styles
* Lack of Flicker
* Full customization

** TEsSwitch ** - Component simulates the eponymous XAML control Windows 10. It has a lot of settings. 
* Complete simulation XAML control 
* Animation 
* Support VCL styles
* Lack of Flicker 
* Full customization of colors 
* Multiple AutoSize-and modes 
* Support ActionList

#Russian ReadMe:
**FreeEsVCLComponents** - это бесплатная библиотека VCL компонентов для Delphi.
Все компоненты FreeEsVCLComponents не мерцают, также не мерцают на них и наследники TGraphiControl при активации свойства BufferedChildens.

**Полностью бесплатная лицензия (В том числе для использования в коммерческих продуктах), открытый исходный код, необходимо только указать а About-е или где-нибудь в другом месте, что в программе используется данный набор компонент.**
**Автор будет благодарен, если будет ссылка на данный репозиторий :)**
**Также нельзя продавать данные компоненты и их производные, что, в общем то логично.**

Плюсы:
* Поддержка Windows XP
* Поддержка встроенного в VCL движка тем
* Удобное отображение границ в DesignTime, при активации свойства IsDrawHelper
* Превосходная поддержка прозрачности
* Возможность сделать прозрачным для мыши, при активации свойства IsTransparentMouse

**TEsCustomControl** - Базовый класс, содержит в себе магию подавления мерцания и поддержки прозрачности.

**TEsNinePatchImage** - TImage для 9-Patch изображений (Растровое изображение с возможностью масштабирования, без потери качества, практически все современные интерфейсы строятся на них), а также изображения - Overley-a

**TEsImageLabel** - Метка с возможностью установки фонового 9-Patch изображения, а также изображения - Overley-a, расположение и выравнивание текста можно комбинировать по-разному

**TEsLayout** - Базовый компонент - контейнер.
Интересен возможностью двойной буферизации для наследников TGraphicControl (TLabel, TGlyphButton, TImage, TBevel...)

**TEsImageLayot** - Layot с поддержкой фоновых 9-Patch изображений, а также изображения - Overley-a

**TEsLabelLayout** - Layot с поддержкой фоновых 9-Patch изображений, а также изображения - Overley-a, и текста расположение и выравнивание текста можно комбинировать по-разному

**TEsActivityBar** - Компонент имитирует XAML контрол Windows 10, созданный для индикации занятости или отображения прогресса (что-то типа TProgressBar).
Имеет множество настроек.
* Поддержка двух режимов отображения
* Три вида анимации и индикация прогресса
* Поддержка VCL тем
* Отсутствие мерцания
* Полная кастомизация

**TEsSwitch** - Компонент имитирует одноименный XAML контрол Windows 10.
Имеет множество настроек.
* Полная имитация XAML контрола
* Анимация
* Поддержка VCL тем
* Отсутствие мерцания
* Полная кастомизация цветов
* Несколько режимов AutoSize-а
* Поддержка ActionList

Пишите ваши отзывы, а также пожелания по улучшению :)

Более оперативно на вопросы я могу ответить здесь: http://vk.com/errorsoft

Совместима с XE2-10
(Хотя класс **TEsCustomControl** будет полезен и ценителям Delphi 7)
