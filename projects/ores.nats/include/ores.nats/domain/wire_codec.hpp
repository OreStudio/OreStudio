/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_NATS_DOMAIN_WIRE_CODEC_HPP
#define ORES_NATS_DOMAIN_WIRE_CODEC_HPP

#include "ores.nats/domain/wire_format.hpp"
#include <cstddef>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <span>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::nats {

/**
 * @brief Encodes/decodes NATS message bodies in a single wire_format
 * fixed at construction.
 *
 * Deliberately non-polymorphic: the format is decided once, at process
 * startup, from the resolved config value (see wire_format.hpp and
 * config::nats_options::wire_format), and never changes for the
 * instance's lifetime. There is no per-message format negotiation, no
 * content-type header, no runtime auto-detection -- every encode()/
 * decode() call branches on the same value for as long as the process
 * runs. wire_codec is a 1-byte enum wrapped in a value type: trivially
 * copyable, cheap to hold by value, reference, or as a member wherever
 * a consumer needs it.
 */
class wire_codec final {
public:
    explicit wire_codec(wire_format format)
        : format_(format) {}

    [[nodiscard]] wire_format format() const {
        return format_;
    }

    /**
     * @brief Serializes @p obj to this codec's fixed wire_format.
     */
    template <typename T>
    [[nodiscard]] std::vector<std::byte> encode(const T& obj) const {
        switch (format_) {
            case wire_format::json: {
                const auto s = rfl::json::write(obj);
                return to_bytes(s);
            }
            case wire_format::msgpack: {
                const auto b = rfl::msgpack::write(obj);
                return to_bytes(b);
            }
        }
        std::unreachable();
    }

    /**
     * @brief Deserializes @p data (assumed to be in this codec's fixed
     * wire_format) into a T.
     */
    template <typename T>
    [[nodiscard]] rfl::Result<T> decode(std::span<const std::byte> data) const {
        switch (format_) {
            case wire_format::json: {
                const std::string_view sv(reinterpret_cast<const char*>(data.data()), data.size());
                return rfl::json::read<T>(sv);
            }
            case wire_format::msgpack:
                return rfl::msgpack::read<T>(data);
        }
        std::unreachable();
    }

private:
    template <typename Container>
    static std::vector<std::byte> to_bytes(const Container& c) {
        const auto* p = reinterpret_cast<const std::byte*>(c.data());
        return {p, p + c.size()};
    }

    wire_format format_;
};

/**
 * @brief Sets the process-wide default wire_codec.
 *
 * Every server process constructs exactly one ores::nats::service::client
 * at startup from its resolved config, and that constructor calls this to
 * publish the codec matching its ORES_NATS_WIRE_FORMAT setting. Generic
 * message-handling code that has no client reference in scope --
 * ores.service::messaging's reply()/decode() choke point, called from
 * hundreds of handler call sites that were never threaded a client or a
 * codec -- reads it back via default_wire_codec() instead of requiring a
 * signature change at every one of those call sites.
 *
 * This is a deliberate, narrow exception to per-instance dependency
 * injection, not a general-purpose global: it is safe precisely because
 * the wire format is a single, process-wide, decided-once-at-startup
 * value (see the write-up task's rejected-alternatives rationale), not a
 * per-instance or per-message choice. Not thread-safe against concurrent
 * set_default_wire_codec() calls from multiple clients disagreeing on
 * format -- no service constructs more than one client with conflicting
 * config, so this does not arise in practice.
 */
ORES_NATS_EXPORT void set_default_wire_codec(wire_codec codec);

/**
 * @brief Returns the process-wide default wire_codec (see
 * set_default_wire_codec()). Defaults to wire_format::json before any
 * client is constructed, matching the pre-existing behaviour for any
 * caller that runs before startup config is applied (e.g. free-standing
 * unit tests).
 */
[[nodiscard]] ORES_NATS_EXPORT const wire_codec& default_wire_codec();

}

#endif
