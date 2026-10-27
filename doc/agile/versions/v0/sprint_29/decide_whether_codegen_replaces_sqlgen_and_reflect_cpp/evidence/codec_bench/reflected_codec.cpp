#include "codecs.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <stdexcept>

namespace bench::reflected {

std::string to_json(const response& v) {
    return rfl::json::write(v);
}

response from_json(std::string_view s) {
    auto r = rfl::json::read<response>(s);
    if (!r)
        throw std::runtime_error(r.error().what());
    return std::move(*r);
}

std::vector<char> to_msgpack(const response& v) {
    return rfl::msgpack::write(v);
}

response from_msgpack(std::span<const char> b) {
    auto r = rfl::msgpack::read<response>(b.data(), b.size());
    if (!r)
        throw std::runtime_error(r.error().what());
    return std::move(*r);
}

}
