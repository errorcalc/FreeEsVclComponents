{******************************************************************************}
{                             FreeEsVclComponents                              }
{                           ErrorSoft(c) 2011-2016                             }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{     errorsoft@protonmail.ch | habrahabr.ru/user/error1024 | errorsoft.org    }
{                                                                              }
{ Open this on github: github.com/errorcalc/FreeEsVclComponents                }
{                                                                              }
{ Вы можете заказать разработку VCL/FMX компонента на заказ                    }
{ You can order the development of VCL/FMX components to order                 }
{******************************************************************************}

// Disable using GdiPlus gaphic library
// WARNING: Base functionality can degrade
{.$define DISABLE_GDIPLUS}

// see TEsBaseLayout.InternalRect
{$define TEST_CONTROL_INTERNAL_RECT}

// see CalcClientRect
{$define FAST_CALC_CLIENTRECT}
