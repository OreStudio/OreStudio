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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_DQ_API_MESSAGING_DATASET_PUBLICATIONS_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_DATASET_PUBLICATIONS_PROTOCOL_HPP

#include "ores.dq.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::dq::messaging {

/**
 * @brief Asks for the runs that published one dataset.
 */
struct dataset_publications_request {
    using response_type = struct dataset_publications_response;
    static constexpr std::string_view nats_subject = "dq.v1.ops.dataset_publications";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The dataset whose runs to read.
     */
    std::string dataset_code;

    friend bool operator==(const dataset_publications_request&,
                           const dataset_publications_request&) = default;
};

/**
 * @brief One run that published a dataset.
 */
struct dataset_publication {
    /**
     * @brief When the run finished, as the store records it.
     */
    std::string published_at;
    /**
     * @brief The person who asked for the run.
     */
    std::string published_by;
    /**
     * @brief How the run wrote its rows: upsert, insert_only or replace_all.
     */
    std::string mode;
    /**
     * @brief The table the run wrote into.
     */
    std::string target_table;
    /**
     * @brief How many rows the run inserted.
     */
    std::int64_t records_inserted = 0;
    /**
     * @brief How many rows the run updated.
     */
    std::int64_t records_updated = 0;
    /**
     * @brief How many rows the run chose not to write.
     */
    std::int64_t records_skipped = 0;
    /**
     * @brief How many rows the run deleted.
     */
    std::int64_t records_deleted = 0;
    /**
     * @brief What the run did: published, published_with_warnings or failed.
     */
    std::string outcome;
    /**
     * @brief What the run said, as the JSON array the log holds.
     */
    std::string messages;

    friend bool operator==(const dataset_publication&, const dataset_publication&) = default;
};

/**
 * @brief Reports the runs, or why they could not be read.
 */
struct dataset_publications_response {
    /**
     * @brief Whether the runs were read.
     */
    bool success = false;
    /**
     * @brief Why the runs could not be read, when they could not.
     */
    std::string error_message;
    /**
     * @brief The runs, newest first.
     */
    std::vector<dataset_publication> publications;

    friend bool operator==(const dataset_publications_response&,
                           const dataset_publications_response&) = default;
};

/**
 * @brief The members of dataset_publications_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_publications_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"dataset_code", &dataset_publications_request::dataset_code},
    };
}

/**
 * @brief The members of dataset_publication, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_publication>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"published_at", &dataset_publication::published_at},
        ::ores::utility::codec::field{"published_by", &dataset_publication::published_by},
        ::ores::utility::codec::field{"mode", &dataset_publication::mode},
        ::ores::utility::codec::field{"target_table", &dataset_publication::target_table},
        ::ores::utility::codec::field{"records_inserted", &dataset_publication::records_inserted},
        ::ores::utility::codec::field{"records_updated", &dataset_publication::records_updated},
        ::ores::utility::codec::field{"records_skipped", &dataset_publication::records_skipped},
        ::ores::utility::codec::field{"records_deleted", &dataset_publication::records_deleted},
        ::ores::utility::codec::field{"outcome", &dataset_publication::outcome},
        ::ores::utility::codec::field{"messages", &dataset_publication::messages},
    };
}

/**
 * @brief The members of dataset_publications_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_publications_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &dataset_publications_response::success},
        ::ores::utility::codec::field{"error_message",
                                      &dataset_publications_response::error_message},
        ::ores::utility::codec::field{"publications", &dataset_publications_response::publications},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_publications_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_publications_request& v);
    static ores::dq::messaging::dataset_publications_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_publications_request& v);
    static ores::dq::messaging::dataset_publications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_publication> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_publication& v);
    static ores::dq::messaging::dataset_publication read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_publication& v);
    static ores::dq::messaging::dataset_publication read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_publications_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_publications_response& v);
    static ores::dq::messaging::dataset_publications_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_publications_response& v);
    static ores::dq::messaging::dataset_publications_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
