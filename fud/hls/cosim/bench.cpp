#include <fstream>
#include <iostream>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "kernel.hpp"

namespace
{

template<class>
struct function_traits;

template<class R, class... Args>
struct function_traits<R (Args...)>
{
    static constexpr std::size_t arity = sizeof...(Args);

    using result_type = R;
    using arguments_type = std::tuple<Args...>;
    using argument_indices = std::make_index_sequence<arity>;
};

using traits = function_traits<decltype(ex0)>;

using result = traits::result_type;
using argv = traits::arguments_type;

template<std::size_t... I>
argv parse_args(const std::string args[], std::index_sequence<I...>)
{
    return {std::tuple_element_t<I, argv>{args[I].c_str()}...};
}

std::vector<argv> read_stimuli(const char *filename)
{
    std::ifstream stream(filename);
    std::string buffers[traits::arity];

    auto read_args = [&]() -> std::istream & {
        for (std::string &buffer : buffers)
            stream >> buffer;

        return stream;
    };

    std::vector<argv> stimuli;

    while (read_args())
        stimuli.push_back(parse_args(buffers, traits::argument_indices{}));

    return stimuli;
}

void write_result(const std::vector<result> &dat, const char *filename)
{
    std::ofstream stream(filename);

    for (const result &r : dat)
        stream << r.to_string(16) << '\n';
}

template<std::size_t... I>
result invoke(const argv &args, std::index_sequence<I...>)
{
    return ex0(std::get<I>(args)...);
}

}   // namespace

int main()
{
    std::vector<result> results;

    for (const argv &args : read_stimuli("sample.dat"))
    {
        results.push_back(invoke(args, traits::argument_indices{}));
    }

    write_result(results, "result.dat");

    return 0;
}
