![Logo](Source/icons/Logo.png)
# ErrorSoft VCL Components
# Alexandria 11.3 Update (**Version 4.4**)
![Preview](Logos/Preview1.gif)
![Preview](Logos/Preview2.gif)
![Preview](Logos/Preview.gif)
---

The old version v2 here: https://github.com/errorcalc/FreeEsVclComponents/tree/Version2  

**If you need to install FreeEsVclComponents in several versions of Delphi, you can use https://github.com/errorcalc/FreeEsVclComponents/blob/master/Packages/ to generate individual packages for every version.**

Free Open Source components for Delphi & C++Builder (VCL)
GetIt and Delphinus-Support
  
## **Is still not know what Delphi/ObjectPascal?**  
**You can get Delphi/Rad Studio IDE on this resource:** https://www.embarcadero.com  
**And learn Delphi/ObjectPascal on this resource:** https://learndelphi.org/  
---
**This library is Open Source software, can be used this in commercial projects, modify, and distribute as source code or binary files.**  
This library licensed at two licenses: Modified MIT License (MIT) and GNU GPL v2.    
You can choose one of two licenses. See "LICENSE.txt"

**You can help, if you send me where you are using the component and attach a screenshot example of use**

**See [ReleaseNotes_4.pdf](Doc/ReleaseNotes_4.pdf) with a description of the new library features!**  

# List of components:
* TEsLayout (Absolutely no flicker! Also for children TGraphicControl`s - BufferedChildrens property)
* TEsPanel (Absolutely no flicker! Also this support new frame styles)
* TEsPaintBox (Flicker free PaintBox)
* TEsShape (Shape with anti-aliasing and opacity)
* TEsImageLayout (Best for your application)
* TEsImageStaticText
* TEsNinePatchImage
* TEsImageLabel
* TEsActivityBar (Looks like Windows 10 XAML control!)
* TEsSwitch (Looks like Windows 10 XAML control!)
* TEsImage (Much better than the TImage)
* TEsImageControl (Much better than the TImage & absolutely no flicker!)
* TEsVirtualImage (TEsImage with TImageCollection support)
* TEsImageControl (Much better than the TImage & absolutely no flicker!)
* TEsVirtualImageControl (TEsImageControl with TImageCollection support)
* TEsRegexpEdit
* TEsRegexButtonedEdit
* TEsRegexLabeledEdit
* TEsLinkLabel
* TEsVersionLabel
* TEsControlListCheckBox (Graphic TCheckBox analog for usage in TControlList)

## XE2, XE3, XE4, XE5, XE6, XE7, XE8, RX10(Seattle), RX10.1(Berlin), RX10.2(Tokyo), RX10.3(Rio), RX10.4(Sydney), RX11(Alexandria) - compatibility

# English ReadMe:

**EsVCLComponents** - Free ErrorSoft Vcl Components - a free library of VCL components for Delphi and C++Builder. This new controls and components to improve the appearance applications and to better user experience. All components have best support transparency, not flicker, and has support Interesting possibility for double buffering for TGraphicControl heirs. Components support visual styles and have a modern style. 

Free use for commercial and non-commercial projects, you need to specify in About, or anywhere else, that the program used components "EsVCLComponents" or link to this repository :)

You can help by giving an example of using the components in your projects(screenshot, description).

You can order developing vcl/fmx components, please submit requests to mail.

**Email:** errorsoft(at)mail.ru or dr.enter256@gmail.com

Features:
* Excellent support for transparency!
* No flicker!
* Support for built-in VCL skins
* Support Hight DPI (Berlin+)
* Display convenient borders in DesignTime, activate properties IsDrawHelper
* Windows XP support

**TEsCustomControl** - The base class contains the anti-flicker magic and support transparency.

![Icon](Source/icons/TESNINEPATCHIMAGE32.bmp)
**TEsNinePatchImage** - TImage for 9-Patch images (raster image with the ability to zoom without loss of quality, almost all modern interfaces are built on them), as well as images - Overlay

![Icon](Source/icons/TESIMAGELABEL32.bmp)
**TEsImageLabel** - The Label supports 9-Patch background image, and image - Overlay, the location and alignment of the text can be combined in different ways

![Icon](Source/icons/TESLAYOUT32.bmp)
**TEsLayout** - The basic container. Interesting possibility for double buffering TGraphicControl heirs (TLabel, TGlyphButton, TImage, TBevel ...)

![Icon](Source/icons/TESPANEL32.bmp)
**TEsPanel** - Flicker free Panel. It’s very nice replacement for TPanel, also this support new frame styles

![Icon](Source/icons/TESSHAPE32.bmp)
**TEsShape** - Shape with anti-aliasing and opacity, also this support new kinds. 

![Icon](Source/icons/TESLINKLABEL32.bmp)
**TEsLinkLabel** - Clickable link, opens a link on the Internet or local

![Icon](Source/icons/TESVERSIONLABEL32.bmp)
**TEsVersionLabel** - Display current version of application

![Icon](Source/icons/TESPAINTBOX32.bmp)
**TEsPaintBox** - Flicker free PaintBox. It’s a very nice replacement for TPaintBox

![Icon](Source/icons/TESIMAGELAYOUT32.bmp)
**TEsImageLayout** - The Layout with support background 9-Patch images and image - Overlay

![Icon](Source/icons/TESIMAGESTATICTEXT32.bmp)
**TEsImageStaticText** - The StaticText supports 9-Patch background image, and image - Overlay, the location and alignment of the text can be combined in different ways

![Icon](Source/icons/TESACTIVITYBAR32.bmp)
**TEsActivityBar** - Component simulates XAML control Windows 10, designed to indicate or display the progress of employment (something TProgressBar type). It has a lot of settings.
* Support for two display modes
* Three types of animation and progress indication
* Support VCL styles
* Lack of Flicker
* Full customization

![Icon](Source/icons/TESSWITCH32.bmp)
**TEsSwitch** - Component simulates the eponymous XAML control Windows 10. It has a lot of settings. 
* Complete simulation XAML control 
* Animation 
* Support VCL styles
* Lack of Flicker 
* Full customization of colors 
* Multiple AutoSize-and modes 
* Support ActionList

![Icon](Source/icons/TESIMAGE32.bmp)
**TEsImage** - Much better than the TImage, many other properties.
* Smoothing(Interpolation) support, this is always lacked TImage
* Support ImageList
* Support Opacity
* More Stretch modes

![Icon](Source/icons/TESIMAGECONTROL32.bmp)
**TEsImageControl** - Much better than the TImage & absolutely no flicker!
* Absolutely no flicker!
* Smoothing(Interpolation) support, this is always lacked TImage
* Support ImageList
* Support Opacity
* More Stretch modes
* Draw frame if need
* Support Focusing

![Icon](Source/icons/TESREGEXEDIT32.bmp)
**TEsRegexEdit** - Edit with regular expression validation and indication.
* Customize colors
* Four indication modes
* Built-in regular expression editor

![Icon](Source/icons/TESREGEXBUTTONEDEDIT32.bmp)
**TEsRegexButtonedEdit** - Buttoned edit with regular expression validation and indication.
* Customize colors
* Four indication modes
* Built-in regular expression editor

![Icon](Source/icons/TESREGEXLABELEDEDIT32.bmp)
**TEsRegexLabeledEdit** - Labeled edit with regular expression validation and indication.
* Customize colors
* Four indication modes
* Built-in regular expression editor

![Icon](Source/icons/TESCONTROLLISTCHECKBOX32.bmp)
**TEsControlListCheckBox** - Graphic TCheckBox analog for usage in TControlList.
* Read-only mode support
* Bindings support

**Look at other projects on this GitHub perhaps you might be interested ...**

# Russian ReadMe:
**ErrorsoftVCLComponents** - Free ErrorSoft Vcl Components - бесплатная библиотека VCL компонентов для Delphi и C++Builder. Это как новые элементы управления, так и компоненты для улучшения внешнего вида приложений и лучшего пользовательского опыта. Все компоненты имеет лучшую поддержку прозрачности, не мерцают, и имеет поддержку интересной возможности - двойная буферизация для наследников TGraphicControl. Компоненты поддерживают визуальные стили и имеет современный стиль. 

**Полностью бесплатная лицензия (в том числе для использования в коммерческих продуктах), открытый исходный код, необходимо только указать а About-е или где-нибудь в другом месте, что в программе используется данный набор компонент.**
**Автор будет благодарен, если будет ссылка на данный репозиторий :)**
**Также нельзя продавать данные компоненты и их производные, что, в общем то логично.**

**Вы можете помочь развитию библиотеки, если вы напишете мне, где вы используете компонент и приложите скриншот с примером использования**

Возможности:
* Превосходная поддержка прозрачности
* Отсутствие мерцания
* Поддержка встроенного в VCL движка тем
* Поддержка Hight DPI (Berlin+)
* Удобное отображение границ в DesignTime, при активации свойства IsDrawHelper
* Поддержка Windows XP

**TEsCustomControl** - Базовый класс, содержит в себе магию подавления мерцания и поддержки прозрачности.

![Icon](Source/icons/TESNINEPATCHIMAGE32.bmp)
**TEsNinePatchImage** - TImage для 9-Patch изображений (Растровое изображение с возможностью масштабирования, без потери качества, практически все современные интерфейсы строятся на них), а также изображения - Overlay-a

![Icon](Source/icons/TESIMAGELABEL32.bmp)
**TEsImageLabel** - Метка с возможностью установки фонового 9-Patch изображения, а также изображения - Overlay-a, расположение и выравнивание текста можно комбинировать

![Icon](Source/icons/TESLAYOUT32.bmp)
**TEsLayout** - Базовый компонент - контейнер.
Интересен возможностью двойной буферизации для наследников TGraphicControl (TLabel, TGlyphButton, TImage, TBevel...)

![Icon](Source/icons/TESPANEL32.bmp)
**TEsPanel** - Свободный от мерцания аналог TPanel. Отличная замена TPanel, поддерживающая новые стили фреймов

![Icon](Source/icons/TESSHAPE32.bmp)
**TEsShape** - Shape with anti-aliasing and opacity, also this support new kinds. 

![Icon](Source/icons/TESLINKLABEL32.bmp)
**TEsLinkLabel** - Кликабельная ссылка, открывает локальную или интернет ссылку

![Icon](Source/icons/TESVERSIONLABEL32.bmp)
**TEsVersionLabel** - Отображает текущую версию приложения

![Icon](Source/icons/TESPAINTBOX32.bmp)
**TEsPaintBox** - PaintBox свободный от мерцания, отличная замена стандартному мерцающему TPaintBox

![Icon](Source/icons/TESIMAGELAYOUT32.bmp)
**TEsImageLayout** - Layot с поддержкой фоновых 9-Patch изображений, а также изображения - Overlay-a

![Icon](Source/icons/TESIMAGESTATICTEXT32.bmp)
**TEsImageStaticText** - StaticText с поддержкой фоновых 9-Patch изображений, а также изображения - Overlay-a, и текста расположение и выравнивание текста можно комбинировать 

![Icon](Source/icons/TESACTIVITYBAR32.bmp)
**TEsActivityBar** - Компонент имитирует XAML контрол Windows 10, созданный для индикации занятости или отображения прогресса (что-то типа TProgressBar).
Имеет множество настроек.
* Поддержка двух режимов отображения
* Три вида анимации и индикация прогресса
* Поддержка VCL тем
* Отсутствие мерцания
* Полная кастомизация

![Icon](Source/icons/TESSWITCH32.bmp)
**TEsSwitch** - Компонент имитирует одноименный XAML контрол Windows 10.
Имеет множество настроек.
* Полная имитация XAML контрола
* Анимация
* Поддержка VCL тем
* Отсутствие мерцания
* Полная кастомизация цветов
* Несколько режимов AutoSize-а
* Поддержка ActionList

![Icon](Source/icons/TESIMAGE32.bmp)
**TEsImage** - Лучшая замена TImage, с большими возможностями.
* Сглаживание, то чего всегда не хватало TImage
* Поддержка ImageList
* Поддержка полупрозрачности
* Несколько режимов растягивания изображения

![Icon](Source/icons/TESIMAGECONTROL32.bmp)
**TEsImageControl** - Лучшая замена TImage, с большими возможностями и с отсутствием мерцания!
* Отсутствие мерцания!
* Сглаживание, то чего всегда не хватало TImage
* Поддержка ImageList
* Поддержка полупрозрачности
* Несколько режимов растягивания изображения
* Рисование границы, если необходимо
* Поддержка фокуса ввода

![Icon](Source/icons/TESREGEXEDIT32.bmp)
**TEsRegexEdit** - TEdit со встроенной поддержкой валидации и индикации правильности ввода с помощью регулярных выражений.
* Настройка цвета
* Четыре режима индикации
* Встроенный редактор регулярных выражений

![Icon](Source/icons/TESREGEXBUTTONEDEDIT32.bmp)
**TEsRegexButtonedEdit** - TButtonedEdit со встроенной поддержкой валидации и индикации правильности ввода с помощью регулярных выражений.
* Настройка цвета
* Четыре режима индикации
* Встроенный редактор регулярных выражений

![Icon](Source/icons/TESREGEXLABELEDEDIT32.bmp)
**TEsRegexLabeledEdit** - TLabeledEdit со встроенной поддержкой валидации и индикации правильности ввода с помощью регулярных выражений.
* Настройка цвета
* Четыре режима индикации
* Встроенный редактор регулярных выражений

![Icon](Source/icons/TESCONTROLLISTCHECKBOX32.bmp)
**TEsControlListCheckBox** - "Графический" аналог TCheckBox для использования с TControlList.
* Поддержка режима "только для чтения"
* Поддержка биндингов

Пишите ваши отзывы, а также пожелания по улучшению :)

**Email:** errorsoft(at)mail.ru

Совместима с Delphi/C++Builder, начиная с XE2
(Хотя класс **TEsCustomControl** будет полезен и ценителям Delphi 7)

**Посмотрите на другие проекты на этом GitHub, возможно, вы будете заинтересованы ...**
