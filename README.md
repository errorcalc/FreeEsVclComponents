![Logo](Source/icons/Logo.png)
# FreeEsVCLComponents
Free components for delphi (VCL)
Attribution-ShareAlike 3.0

# List of components:
* TEsLayout (Absolitly no flicker! Also for children TGraphicControl`s - BufferedChildrens property)
* TEsImageLayout (Best for your application)
* TEsLabelLayout
* TEsNinePathImage
* TEsImageLabel
* TEsActivityBar (looks like Windows 10 XAML control!)

# XE2, XE3, XE4, XE5, XE6, XE7, XE8, RX10(Seattle) - compatibility

#Russian ReadMe:
**FreeEsVCLComponents** - это бесплатная библиотека VCL компонентов для Delphi.
Все компоненты FreeEsVCLComponents не мерцают, также не мерцают на них и наследники TGraphiControl при активации свойства BufferedChildens.

Плюсы:
* Поддержка Windows XP
* Поддержка встроенного в VCL движка тем
* Удобное отображение границ в DesignTime, при активации свойства IsDrawHelper
* Превосходная поддержка прозрачности
* Возможность сделать прозрачным для мыши, при активации свойства IsTransparentMouse

**TEsCustomControl** - Базовый класс, содержит в себе магию подавления мерцания и поддержки прозрачности.

**TEsNinePathImage** - TImage для 9-Path изображений (Растровое изображение с возможностью масштабирования, без потери качества, практически все современные интерфейсы строятся на них), а также изображения - Overley-a

**TEsImageLabel** - Метка с возможностью установки фонового 9-Path изображения, а также изображения - Overley-a, расположение и выравнивание текста можно комбинировать по-разному

**TEsLayout** - Базовый компонент - контейнер.
Интересен возможностью двойной буферизации для наследников TGraphicControl (TLabel, TGlyphButton, TImage, TBevel...)

**TEsImageLayot** - Layot с поддержкой фоновых 9-Path изображений, а также изображения - Overley-a

**TEsLabelLayout** - Layot с поддержкой фоновых 9-Path изображений, а также изображения - Overley-a, и текста расположение и выравнивание текста можно комбинировать по-разному

**TEsActivityBar** - Компонент имитирует XAML контрол Windows 10, созданный для индикации занятости или отображения прогресса (что-то типа TProgressBar).
Имеет множество настроек.
* Поддержка двух режимов отображения
* Три вида анимации и индикация прогресса
* Поддержка VCL тем
* Отсутствие мерцания
* Полная кастомизация

Пишите ваши отзывы, а также пожелания по улучшению :)

Более оперативно на вопросы я могу ответить здесь: http://vk.com/errorsoft

Совместима с XE2-10
(Хотя класс **TEsCustomControl** будет полезен и ценителям Delphi 7)
