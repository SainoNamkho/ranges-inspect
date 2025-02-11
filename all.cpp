#include <ranges>
#include <vector>
#include <print>
#include <source_location>
#include <string_view>
#include <utility>
#include <tuple>    
#include <string>
#include <memory>

#if defined(__GNUC__)
#if __has_feature(reflection)
#include <experimental/meta>
#endif
#endif

// Ranges formatter polyfill
#ifndef __cpp_lib_format_ranges
// We only support char, `CharT` is used to avoid ambiguous template partial specializations on MS STL
template<std::ranges::input_range R, class CharT>
struct std::formatter<R, CharT> : std::formatter<ranges::range_value_t<R>, CharT>
{
    using F = std::formatter<std::ranges::range_value_t<R>>;
    
    // In case `r` is of `const R`, but `begin` or `end` is not marked const
    auto format(ranges::input_range auto&& r, auto& fmt_ctx) const -> decltype(fmt_ctx.out()) {
        auto out = fmt_ctx.out();
        *out++ = '[';
        auto begin = std::ranges::begin(r);
        auto end = std::ranges::end(r);
        if (begin != end) {
            fmt_ctx.advance_to(out);
            out = F::format(*r.begin(), fmt_ctx);              
        
            for (auto&& x : ranges::subrange{next(begin), end}) {
                out = ranges::copy(", ", out).out;
                fmt_ctx.advance_to(out);
                out = F::format(x, fmt_ctx);
            }
        }
        *out++ = ']';
        return out;
    }
};

#ifndef _MSC_VER
template<template<class...> class Tuple, class... Ts> requires std::same_as<Tuple<Ts...>, std::tuple<Ts...>> || std::same_as<Tuple<Ts...>, std::pair<Ts...>>
struct std::formatter<Tuple<Ts...>>
{
    std::tuple<std::formatter<remove_cvref_t<Ts>>...> base;
    constexpr auto parse(auto& ctx) -> decltype(ctx.begin()) {
        apply([&](auto&... args) {
            (ctx.advance_to(args.parse(ctx)), ...);
        }, base);
        return ctx.begin();
    }
    
    auto format(Tuple<Ts...>&& t, auto& ctx) const -> decltype(ctx.out()) {
        auto out = ctx.out();
        *out++ = '(';
        if constexpr (sizeof...(Ts) != 0) {
            ctx.advance_to(out);
            ctx.advance_to(get<0>(base).format(get<0>(t), ctx));
            [&]<size_t... Is>(index_sequence<0, Is...>) {
                ((ctx.advance_to(
                    ranges::copy(", ", ctx.out()).out
                ), 
                ctx.advance_to(
                    get<Is>(base).format(get<Is>(t), ctx)
                )),
                ...);
            }(index_sequence_for<Ts...>{});
        }
        out = ctx.out();
        *out++ = ')';
        return out;
    }
};
#endif
#endif

// You may define it to print more rare info, which might be helpful if my code brings additional bugs.
// #define DEVELOPING
#ifdef DEVELOPING
#define __debug_println std::println
#define __debug_print std::print
#else
#define __debug_println(...) void()
#define __debug_print(...) void()
#endif

namespace range_debug {
using namespace std::literals;
auto debug_on_begin = false;
#if defined(__GNUC__) || defined(__clang__)
auto indent = 0uz;
#else
auto indent = std::size_t{0};
#endif

template<std::ranges::range R>
void range_print(R&& r);

// Design goal: Try to make `debug_view<R>` as transparent to `R` as possible.
// `viewable_range` is required to be suitable for `views::all`
template<std::ranges::viewable_range R>
struct debug_view : R {
    using debug_view_tag = void;
    using Base = R;

    template<class Self> // requires std::same_as<std::remove_cvref_t<Self>, debug_view> 
    constexpr decltype(auto) base_range(this Self&& self) noexcept {
        return static_cast<decltype(std::forward_like<Self>(std::declval<R>()))>(self);
    }

    // `R` shouble be convertible to `debug_view` since we injected `ref_view` and `owning_view`
    using R::R;
    debug_view() = default;
    debug_view(const debug_view&) = default;
    debug_view(debug_view&&) = default;
    debug_view(const R& r) : Base(r) {}
    debug_view(R&& r) : Base(std::move(r)) {}

    // to model std::movable
    debug_view& operator=(const debug_view&) = default;
    debug_view& operator=(debug_view&&) = default;

    // Implicit object parameter is more specialize than the templated parameter defined in operator| for range adaptor closures.
    // Derived classes should explicitly inherit them.
    // However msvc does not resolve overloads correctly.
#ifndef _MSC_VER
    template<class Closure>
    constexpr decltype(auto) operator|(Closure&& closure) & noexcept(noexcept(range_debug::debug_view(std::forward<Closure>(closure)(base_range())))) {
        return range_debug::debug_view(std::forward<Closure>(closure)(base_range()));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(Closure&& closure) && noexcept(noexcept(range_debug::debug_view(std::forward<Closure>(closure)(std::move(*this).base_range())))) {
        return range_debug::debug_view(std::forward<Closure>(closure)(std::move(*this).base_range()));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(Closure&& closure) const& noexcept(noexcept(range_debug::debug_view(std::forward<Closure>(closure)(base_range())))) {
        return range_debug::debug_view(std::forward<Closure>(closure)(base_range()));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(Closure&& closure) const&& noexcept(noexcept(range_debug::debug_view(std::forward<Closure>(closure)(std::move(*this).base_range())))) {
        return range_debug::debug_view(std::forward<Closure>(closure)(std::move(*this).base_range()));
    }
#else
    template<class Closure>
    constexpr decltype(auto) operator|(this debug_view& self, Closure&& closure) noexcept(noexcept(range_debug::debug_view(std::forward<Closure>(closure)(self.base_range())))) {
        return range_debug::debug_view(std::forward<Closure>(closure)(self.base_range()));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(this debug_view&& self, Closure&& closure) noexcept(noexcept(range_debug::debug_view(std::forward<Closure>(closure)(std::move(self).base_range())))) {
        return range_debug::debug_view(std::forward<Closure>(closure)(std::move(self).base_range()));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(this const debug_view& self, Closure&& closure) noexcept(noexcept(range_debug::debug_view(std::forward<Closure>(closure)(self.base_range())))) {
        return range_debug::debug_view(std::forward<Closure>(closure)(self.base_range()));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(this const debug_view&& self, Closure&& closure) noexcept(noexcept(range_debug::debug_view(std::forward<Closure>(closure)(std::move(self).base_range())))) {
        return range_debug::debug_view(std::forward<Closure>(closure)(std::move(self).base_range()));
    }
#endif

#if defined(DEVELOPING) || !defined(_LIBCPP_EXPERIMENTAL_META)
    // We have to lose constexprness without reflection
    template<class Self> requires /*std::same_as<std::remove_cvref_t<Self>, debug_view> &&*/ requires {
        // To be sfinae friendly, e.g., to `filter_view::begin` which is not marked `const`,
        // thus might cause hard error when checked by some internal implementation in the ranges library otherwise.
        std::declval<Self>().base_range().begin();
    }
    auto begin(this Self&& self) {
        __debug_println("begin: debug :{}, in {}", debug_on_begin, std::source_location::current().function_name());
        auto flag = debug_on_begin;
        auto _ = std::unique_ptr<bool, decltype([](bool* old) {
            debug_on_begin = *old;
        })>{&flag};
        if (debug_on_begin) {
            debug_on_begin = false;
            range_print(std::forward<Self>(self).base_range());
        }
        return std::forward<Self>(self).base_range().begin();
    }
#endif
};

// `all_t` is still need to cope with pathological ranges.
// We make sure `all_t` is not injected when `T` is already (possibly derived from) `debug_view`
template<class T>
debug_view(T&&) -> debug_view<std::views::all_t<T>>;

// Now no need to `V | debug | other_closures ...`, works as a workaround if our automatic injection not working well.
struct debug_view_fn : std::ranges::range_adaptor_closure<debug_view_fn>
{
    template<class R>
    constexpr auto operator()(R&& r) const noexcept(noexcept(debug_view(std::forward<R>(r)))) {
        return debug_view(std::forward<R>(r));
    }
} debug;

template<std::ranges::viewable_range>
struct debug_view_wrap_tag{
    // to model `range<R>` for `ref_view<R>`
    constexpr int* begin() const { return nullptr; }
    constexpr int* end() const { return nullptr; }
};

template<class V>
concept debuggable = requires { typename std::remove_cvref_t<V>::debug_view_tag; };

template<class V>
concept nondebuggable = !debuggable<V>;

template<auto>
struct A{};

template<class T>
consteval auto type_name_misc() -> std::string_view
{
    return std::source_location::current().function_name();
}

// Example: `r | closure` being `XXXView<all_t<R>>` (aka `XXXView<ref_view<debug_view_wrap_tag<R>>`),
//          which originally models `view`, but is not inherited from `debug_view`.
// We specialize `enable_view` for them to go through `ref_view` injection.
template<class V>
concept pseudo_debuggable = nondebuggable<V> &&
#ifdef __clang__
    (type_name_misc<V>().contains("std::ranges::ref_view<range_debug::debug_view"sv) ||
     type_name_misc<V>().contains("std::ranges::owning_view<range_debug::debug_view"sv));
#elifdef __GNUC__
    (type_name_misc<V>().contains("std::ranges::ref_view<debug_view"sv) ||
     type_name_misc<V>().contains("std::ranges::owning_view<debug_view"sv));
#else // msvc
    (type_name_misc<V>().contains("struct std::ranges::ref_view<struct range_debug::debug_view"sv) ||
     type_name_miszc<V>().contains("struct std::ranges::owning_view<struct range_debug::debug_view"sv));
#endif


// Users may specialize this or define FORCE_INSPECT to force inspect an input range, which may be unsafe.
template<class T>
constexpr bool force_inspect = false;

#ifdef FORCE_INSPECT
template<class T> requires true
constexpr bool force_inspect<T> = true;
#endif

template<class R>
void print_view_type()
{
#ifdef __clang__
    constexpr auto prefix = "void range_debug::print_view_type() [R = "sv;
    constexpr auto suffix = "]"sv;
#elifdef __GNUC__
    constexpr auto prefix = "void range_debug::print_view_type() [with R = "sv;
    constexpr auto suffix = "]"sv;
#else // msvc
    constexpr auto prefix = "void __cdecl range_debug::print_view_type<"sv;
    constexpr auto suffix = ">(void)"sv;
#endif
    constexpr auto name = std::string_view{std::source_location::current().function_name()}.substr(prefix.size());
    auto raw_name = std::string{name};

    auto remove_debug_info = [&raw_name](std::string_view debug, int level) {
        auto str = std::string{raw_name};
        auto next = std::string{};
        auto sv = std::string_view{str};
        if (auto idx = sv.find(debug); idx != std::string_view::npos) {
            next.append(sv.substr(0, idx));
            sv.remove_prefix(idx + debug.size());
            auto close_angle = [sv] {
                for (int angle_cnt = 0;  auto& c : sv) {
                    if (c == '<')
                        angle_cnt++;
                    if (c == '>')
                        angle_cnt--;
                    if (angle_cnt < 0)
                        return &c;
                }
                std::unreachable();
            }();
            --level;

            // collapse references
            std::size_t c1{}, c2{};
            auto p1 = close_angle;
            while ("& "sv.contains(*p1)) {
                if (*p1-- == '&')
                    c1++;
            }
            if (level != 0)
                while (*++close_angle != '>' || --level != 0);
            auto p2 = close_angle + 1;
            while (p2 != std::to_address(sv.end()) && "& "sv.contains(*p2)) {
                if (*p2++ == '&')
                    c2++;
            }
            next.append(sv.data(), p1);
            next.append(c1 || c2 ? c1 == 1 || c2 == 1 ? 1 : 2 : 0, '&');
            sv.remove_prefix(p2 - sv.data());
        }
        next.append(sv);
        if (next.size() != raw_name.size()) {
            raw_name = std::move(next);
            return true;
        }
        return false;
    };
#ifdef __clang__
    while (remove_debug_info("range_debug::debug_view<"sv, 1) ||
           remove_debug_info("std::ranges::ref_view<range_debug::debug_view_wrap_tag<"sv, 2) ||
           remove_debug_info("std::ranges::owning_view<range_debug::debug_view_wrap_tag<"sv, 2));
#elifdef __GNUC__
    while (remove_debug_info("debug_view<"sv, 1) ||
           remove_debug_info("std::ranges::ref_view<debug_view_wrap_tag<"sv, 2) ||
           remove_debug_info("std::ranges::owning_view<debug_view_wrap_tag<"sv, 2));
#else // msvc
    while (remove_debug_info("struct range_debug::debug_view<"sv, 1) ||
           remove_debug_info("struct std::ranges::ref_view<struct range_debug::debug_view_wrap_tag<"sv, 2) ||
           remove_debug_info("struct std::ranges::owning_view<struct range_debug::debug_view_wrap_tag<"sv, 2));
#endif
    // std::println("{}", display_string_of(^^R));
    __debug_println("{}\x1b[31m{}", std::string(indent, ' '), name.substr(0, name.size() - suffix.size()));
    std::println("{}\x1b[34m{}\x1b[m", std::string(indent, ' '), std::string_view{raw_name}.substr(0, raw_name.size() - suffix.size()));
}

template<std::ranges::range R>
void range_print(R&& r)
{
    __debug_println("print: debug: {}, in {}", debug_on_begin, std::source_location::current().function_name());
    if constexpr (debuggable<R>) {
        __debug_println("forward base");
        range_print(std::forward<R>(r).base_range());
    }
    else {
        print_view_type<R>();
        __debug_println("members, debug: {}", debug_on_begin);
        if constexpr (std::ranges::forward_range<R&> || force_inspect<std::remove_cvref_t<R>>) {
            std::println("{}{}", std::string(indent, ' '), std::forward<R>(r));
        }
        if constexpr (requires { range_print(r.base()); }) {
            range_print(r.base());
        }
#ifdef _LIBCPP_EXPERIMENTAL_META
        else if constexpr (constexpr auto info = [] {
            auto members = nonstatic_data_members_of(remove_cvref(^^R));
            auto bases = std::ranges::find_if(members, [](auto info) {
                return has_template_arguments(type_of(info)) && template_of(type_of(info)) == ^^std::tuple;
            });
            return bases != members.end() ? *bases : ^^::;
        }(); info != ^^::) {
            indent += 2;
            auto&& bases = r.[:info:];
            [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                (range_print(std::get<Is>(bases)),...);
            }(std::make_index_sequence<std::tuple_size_v<std::remove_cvref_t<decltype(bases)>>>{});
            indent -= 2;
        }
#else
        else {
            debug_on_begin = true;
            indent += 2;
            (void)std::forward<R>(r).begin();
            __debug_println("[OK] in {}", std::source_location::current().function_name());
            indent -= 2;
            debug_on_begin = false;
        }
#endif
    }
}
}

// clang produces different names when namespace `  A::inline B::C` is
// reopened as `namespace A::C` somewhere
#ifdef _LIBCPP_VERSION
    _LIBCPP_BEGIN_NAMESPACE_STD
#else
    namespace std {
    #ifdef __GLIBCXX__
        _GLIBCXX_BEGIN_NAMESPACE_VERSION
    #endif
#endif
namespace ranges {
template<::range_debug::debuggable R>
constexpr bool enable_borrowed_range<R> = enable_borrowed_range<typename R::Base>;

// `R` is already decayed by `views::all`
template<::range_debug::pseudo_debuggable R>
constexpr bool enable_view<R> = false;

template<::range_debug::nondebuggable R>
ref_view(R&) -> ref_view<::range_debug::debug_view<R>>;

// `R&` vs `R` is ambiguous, so we split this `R` into `R&` and `R&&`.
template<::range_debug::pseudo_debuggable R>
ref_view(R&) -> ref_view<::range_debug::debug_view_wrap_tag<R>>;

template<::range_debug::pseudo_debuggable R>
ref_view(R&&) -> ref_view<::range_debug::debug_view_wrap_tag<std::remove_cvref_t<R>>>;

template<class R>
struct ref_view<::range_debug::debug_view<R>> : ::range_debug::debug_view<ref_view<R>>
{
    using Base = /* make gcc happy */typename ref_view::debug_view;
    using Base::Base;
#ifndef _MSC_VER
    using Base::operator|;
#else
    template<class Closure>
    constexpr decltype(auto) operator|(this ref_view& self, Closure&& closure) noexcept(noexcept(static_cast<Base&>(self).operator|(std::forward<Closure>(closure)))) {
        return static_cast<Base&>(self).operator|(std::forward<Closure>(closure));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(this ref_view&& self, Closure&& closure) noexcept(noexcept(static_cast<Base&&>(self).operator|(std::forward<Closure>(closure)))) {
        return static_cast<Base&&>(self).operator|(std::forward<Closure>(closure));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(this const ref_view& self, Closure&& closure) noexcept(noexcept(static_cast<const Base&>(self).operator|(std::forward<Closure>(closure)))) {
        return static_cast<const Base&>(self).operator|(std::forward<Closure>(closure));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(this const ref_view&& self, Closure&& closure) noexcept(noexcept(static_cast<const Base&&>(self).operator|(std::forward<Closure>(closure)))) {
        return static_cast<const Base&&>(self).operator|(std::forward<Closure>(closure));
    }
#endif
};

// `pseudo_debuggable` `R`s are actually views, so `debug_view<R>` is valid.
template<::range_debug::pseudo_debuggable R>
struct ref_view<::range_debug::debug_view_wrap_tag<R>> : ::range_debug::debug_view<R>
{
    using Base = typename ref_view::debug_view;
    using Base::Base;
#ifndef _MSC_VER
    using Base::operator|;
#else
    template<class Closure>
    constexpr decltype(auto) operator|(this ref_view& self, Closure&& closure) noexcept(noexcept(static_cast<Base&>(self).operator|(std::forward<Closure>(closure)))) {
        return static_cast<Base&>(self).operator|(std::forward<Closure>(closure));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(this ref_view&& self, Closure&& closure) noexcept(noexcept(static_cast<Base&&>(self).operator|(std::forward<Closure>(closure)))) {
        return static_cast<Base&&>(self).operator|(std::forward<Closure>(closure));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(this const ref_view& self, Closure&& closure) noexcept(noexcept(static_cast<const Base&>(self).operator|(std::forward<Closure>(closure)))) {
        return static_cast<const Base&>(self).operator|(std::forward<Closure>(closure));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(this const ref_view&& self, Closure&& closure) noexcept(noexcept(static_cast<const Base&&>(self).operator|(std::forward<Closure>(closure)))) {
        return static_cast<const Base&&>(self).operator|(std::forward<Closure>(closure));
    }
#endif
};

template<class R> requires (!::range_debug::debuggable<R>)
owning_view(R) -> owning_view<::range_debug::debug_view<R>>;

template<class R>
struct owning_view<::range_debug::debug_view<R>> : ::range_debug::debug_view<owning_view<R>>
{
    using Base = typename owning_view::debug_view;
    using Base::Base;
#ifndef _MSC_VER
    using Base::operator|;
#else
    template<class Closure>
    constexpr decltype(auto) operator|(this owning_view& self, Closure&& closure) noexcept(noexcept(static_cast<Base&>(self).operator|(std::forward<Closure>(closure)))) {
        return static_cast<Base&>(self).operator|(std::forward<Closure>(closure));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(this owning_view&& self, Closure&& closure) noexcept(noexcept(static_cast<Base&&>(self).operator|(std::forward<Closure>(closure)))) {
        return static_cast<Base&&>(self).operator|(std::forward<Closure>(closure));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(this const owning_view& self, Closure&& closure) noexcept(noexcept(static_cast<const Base&>(self).operator|(std::forward<Closure>(closure)))) {
        return static_cast<const Base&>(self).operator|(std::forward<Closure>(closure));
    }
    template<class Closure>
    constexpr decltype(auto) operator|(this const owning_view&& self, Closure&& closure) noexcept(noexcept(static_cast<const Base&&>(self).operator|(std::forward<Closure>(closure)))) {
        return static_cast<const Base&&>(self).operator|(std::forward<Closure>(closure));
    }
#endif
};
}
#ifdef _LIBCPP_VERSION
    _LIBCPP_END_NAMESPACE_STD
#elifdef __GLIBCXX__
    _GLIBCXX_END_NAMESPACE_VERSION }
#else
    }
#endif



int main()
{
    using namespace std::views;
    //using namespace std::ranges;
    using namespace range_debug;
    auto v = std::vector{1, 2, 3, 4, 5};
    auto v1 = v //| debug
                | filter([](auto&& x) { return x % 2; })
                | transform([](auto&& x) { return 1; });
    // auto x = all(v);
    // auto lmd = [](auto&& x) { return x % 2; };
    // auto y = filter(std::move(lmd));
    // auto z = transform([](auto&& x) { return 1; });
    // static_assert(std::same_as<
    //     decltype(v | std::move(y) | std::move(z)),
    //     decltype(x | std::move(y) | std::move(z))
    // >);
    // std::ranges::ref_view{v | std::move(y) };
    // static_assert(pseudo_debuggable<decltype(std::ranges::filter_view(v, std::move(lmd)))>);
    // static_assert(!std::ranges::view<decltype(v1)>);
    // std::println("{}", type_name_misc<decltype(all)>());
    // std::ranges::ref_view{v1};
    range_print(v1);
    range_print(zip(v, v1));
}
