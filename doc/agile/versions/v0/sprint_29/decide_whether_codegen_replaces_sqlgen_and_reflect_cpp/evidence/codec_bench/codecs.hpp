#ifndef CODEC_BENCH_CODECS_HPP
#define CODEC_BENCH_CODECS_HPP

#include "ores.dq.api/messaging/change_reason_category_protocol.hpp"
#include <span>
#include <string>
#include <vector>

namespace bench {

using response = ores::dq::messaging::list_change_reason_categories_response;

/*
 * The two codecs under comparison. Each lives in its own translation unit, so
 * the build also measures what each costs to compile.
 */
namespace reflected {
std::string to_json(const response& v);
response from_json(std::string_view s);
std::vector<char> to_msgpack(const response& v);
response from_msgpack(std::span<const char> b);
}

namespace generated {
std::string to_json(const response& v);
response from_json(std::string_view s);
std::vector<char> to_msgpack(const response& v);
response from_msgpack(std::span<const char> b);
}

}

#endif
