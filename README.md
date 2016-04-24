![Logo](Source/icons/Logo.png)
# FreeEsVCLComponents
**Version 1.0**

Free components for delphi & C++Builder (VCL)
Attribution-ShareAlike 3.0

*** You can help, if you send me where you are using the component and attach a screenshot example of use ***

*** Вы можете помочь, если вы напишете мне, где вы используете компонент и приложите скриншот с примером использования ***

# List of components:
* TEsLayout (Absolitly no flicker! Also for children TGraphicControl`s - BufferedChildrens property)
* TEsImageLayout (Best for your application)
* TEsImageStaticText
* TEsNinePatchImage
* TEsImageLabel
* TEsActivityBar (looks like Windows 10 XAML control!)
* TEsSwitch (looks like Windows 10 XAML control!)
* TEsImage (much better than the TImage)
* TEsImageControl (much better than the TImage & absolitly no flicker!)

# XE2, XE3, XE4, XE5, XE6, XE7, XE8, RX10(Seattle), RX10.1(Berlin) - compatibility
# Now: And support C++Builder

#English ReadMe:

** FreeEsVCLComponents ** - a free library of VCL components for Delphi. All components has best support transparency, FreeEsVCLComponents not flicker, support Interesting possibility for double buffering for TGraphicControl heirs - BufferedChildens property.

Free use for commercial and non-commercial projects, you need specify in About, or anywhere else, that the program used components "FreeEsVclComponents" or link to this repository :)

errorsoft(at)mail.ru

**Units was renamed!(for C++Builder Support) Need rename in your project "ES.Vcl.xxx.pas -> ES.xxx.pas"**

Pluses:
* Windows XP support
* Support for built-in VCL skins
* Support Hight DPI (Seattle+)
* Display convenient borders in DesignTime, activate properties IsDrawHelper
* Excellent support for transparency!

**TEsCustomControl** - The base class contains the anti-flicker magic and support transparency.

**TEsNinePatchImage** - TImage for 9-Patch images (raster image with the ability to zoom without loss of quality, almost all modern interfaces are built on them), as well as images - Overley

**TEsImageLabel** - The Label supports 9-Patch background image, and image - Overley, the location and alignment of the text can be combined in different ways

**TEsLayout** - The basic container. Interesting possibility for double buffering TGraphicControl heirs (TLabel, TGlyphButton, TImage, TBevel ...)

**TEsImageLayot** - The Layout with support background 9-Patch images and image - Overley

**TEsImageStaticText** - The StaticText supports 9-Patch background image, and image - Overley, the location and alignment of the text can be combined in different ways

**TEsActivityBar** - Component simulates XAML control Windows 10, designed to indicate or display the progress of employment (something TProgressBar type). It has a lot of settings.
* Support for two display modes
* Three types of animation and progress indication
* Support VCL styles
* Lack of Flicker
* Full customization

**TEsSwitch** - Component simulates the eponymous XAML control Windows 10. It has a lot of settings. 
* Complete simulation XAML control 
* Animation 
* Support VCL styles
* Lack of Flicker 
* Full customization of colors 
* Multiple AutoSize-and modes 
* Support ActionList

**TEsImage** - Much better than the TImage, many other properties.
* Smoothing(Interpolation) support, this is always lacked TImage
* Support ImageList
* Support Opacity
* More Stretch modes

**TEsImageControl** - Much better than the TImage & absolitly no flicker!
* Absolitly no flicker!
* Smoothing(Interpolation) support, this is always lacked TImage
* Support ImageList
* Support Opacity
* More Stretch modes
* Draw frame if need
* Support Focusing

#Russian ReadMe:
**FreeEsVCLComponents** - это бесплатная библиотека VCL компонентов для Delphi.
Все компоненты FreeEsVCLComponents не мерцают, также не мерцают на них и наследники TGraphiControl при активации свойства BufferedChildens.

**Полностью бесплатная лицензия (В том числе для использования в коммерческих продуктах), открытый исходный код, необходимо только указать а About-е или где-нибудь в другом месте, что в программе используется данный набор компонент.**
**Автор будет благодарен, если будет ссылка на данный репозиторий :)**
**Также нельзя продавать данные компоненты и их производные, что, в общем то логично.**

**Модули были переименованы!(Для поддержки C++Builder) Необходимо исправить названия "ES.Vcl.xxx -> ES.xxx"**

Плюсы:
* Поддержка Windows XP
* Поддержка встроенного в VCL движка тем
* Поддержка Hight DPI (Seattle+)
* Удобное отображение границ в DesignTime, при активации свойства IsDrawHelper
* Превосходная поддержка прозрачности

**TEsCustomControl** - Базовый класс, содержит в себе магию подавления мерцания и поддержки прозрачности.

**TEsNinePatchImage** - TImage для 9-Patch изображений (Растровое изображение с возможностью масштабирования, без потери качества, практически все современные интерфейсы строятся на них), а также изображения - Overley-a

**TEsImageLabel** - Метка с возможностью установки фонового 9-Patch изображения, а также изображения - Overley-a, расположение и выравнивание текста можно комбинировать по-разному

**TEsLayout** - Базовый компонент - контейнер.
Интересен возможностью двойной буферизации для наследников TGraphicControl (TLabel, TGlyphButton, TImage, TBevel...)

**TEsImageLayot** - Layot с поддержкой фоновых 9-Patch изображений, а также изображения - Overley-a

**TEsImageStaticText** - StaticText с поддержкой фоновых 9-Patch изображений, а также изображения - Overley-a, и текста расположение и выравнивание текста можно по-разному комбинировать 

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

**TEsImage** - Much better than the TImage, many other properties.
* Сглаживание, то чего всегда не хватало TImage
* Поддержка ImageList
* Поддержка полупрозрачности
* Несколько режимов растягивания изображения

**TEsImageControl** - Much better than the TImage & absolitly no flicker!
* Отсутствие мерцания!
* Сглаживание, то чего всегда не хватало TImage
* Поддержка ImageList
* Поддержка полупрозрачности
* Несколько режимов растягивания изображения
* Рисование границы, если необходимо
* Поддежка фокуса ввода

Пишите ваши отзывы, а также пожелания по улучшению :)

errorsoft(at)mail.ru

Более оперативно на вопросы я могу ответить здесь: http://vk.com/errorsoft

Совместима с XE2-10.1
(Хотя класс **TEsCustomControl** будет полезен и ценителям Delphi 7)
