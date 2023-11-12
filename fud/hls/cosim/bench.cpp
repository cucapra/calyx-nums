#include <array>
#include <fstream>
#include <iostream>
#include <string>
#include <type_traits>
#include <vector>

#include "kernel.hpp"
#include "polyfills.hpp"

namespace
{

template<class>
struct arity {};

template<class R, class... Args>
struct arity<R (Args...)> :
    std::integral_constant<std::size_t, sizeof...(Args)>
{
};

using argv = std::array<float, arity<decltype(ex0)>::value>;

std::vector<argv> read_stimuli(const char *filename)
{
    std::vector<argv> table;
    argv row;

    std::ifstream stream(filename);
    std::string buffer;

    for (std::size_t i = 0; stream >> buffer; )
    {
        row[i] = std::stof(buffer);

        if (++i == row.size())
        {
            table.push_back(row);
            i = 0;
        }
    }

    return table;
}

void write_result(const std::vector<float> &dat, const char *filename)
{
    std::ofstream stream(filename);

    stream.setf(std::ios_base::fixed | std::ios_base::scientific,
                std::ios_base::floatfield);

    for (float r : dat)
    {
        stream << r << '\n';
    }
}

template<std::size_t... I>
float invoke(const argv &args, cxx14::index_sequence<I...>)
{
    return ex0(std::get<I>(args)...);
}

float invoke(const argv &args)
{
    using size = std::tuple_size<argv>;
    using indices = cxx14::make_index_sequence<size::value>;

    return invoke(args, indices{});
}

}   // namespace

int main()
{
    std::vector<float> result;

    for (const argv &args : read_stimuli("sample.dat"))
    {
        result.push_back(invoke(args));
    }

    write_result(result, "result.dat");

    return 0;
}
