#ifndef POLYFILLS_HPP
#define POLYFILLS_HPP

#include <cstddef>

namespace cxx14
{

template<std::size_t... Ints>
struct index_sequence {};

namespace detail
{

template<std::size_t N, std::size_t... Ints>
struct make_index_sequence :
    make_index_sequence<N - 1, N - 1, Ints...>
{
};

template<std::size_t... Ints>
struct make_index_sequence<0, Ints...>
{
    using type = index_sequence<Ints...>;
};

}   // namespace detail

template<std::size_t N>
using make_index_sequence = typename detail::make_index_sequence<N>::type;

}   // namespace cxx14

#endif
