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
#ifndef ORES_REFDATA_API_MESSAGING_REGULATORY_CORPORATE_SECTOR_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_REGULATORY_CORPORATE_SECTOR_PROTOCOL_HPP

#include "ores.refdata.api/domain/regulatory_corporate_sector.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::refdata::messaging {

struct regulatory_corporate_sector_key {
    std::string code;
    std::string coding_scheme_code;
};

struct regulatory_corporate_sector_write {
    std::string code;
    std::string coding_scheme_code;
    std::string source;
    std::string description;
};

struct regulatory_corporate_sector_change {
    regulatory_corporate_sector_write write;
    ores::utility::domain::precondition precondition;
};

struct regulatory_corporate_sector_removal {
    regulatory_corporate_sector_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct regulatory_corporate_sector_lookup {
    regulatory_corporate_sector_key key;
    std::optional<ores::refdata::domain::regulatory_corporate_sector> regulatory_corporate_sector;
};

struct regulatory_corporate_sector_event {
    boost::uuids::uuid event_id;
    regulatory_corporate_sector_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct regulatory_corporate_sector_version_key {
    regulatory_corporate_sector_key regulatory_corporate_sector;
    std::uint32_t version;
};

struct regulatory_corporate_sector_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_regulatory_corporate_sectors_request {
    using response_type = struct list_regulatory_corporate_sectors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.regulatory_corporate_sectors.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<std::string> as_of;
};

struct list_regulatory_corporate_sectors_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::regulatory_corporate_sector> sectors;
    std::uint64_t total;
};

struct get_regulatory_corporate_sector_request {
    using response_type = struct get_regulatory_corporate_sector_response;
    static constexpr std::string_view nats_subject = "refdata.v1.regulatory_corporate_sectors.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    regulatory_corporate_sector_key key;
};

struct get_regulatory_corporate_sector_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::regulatory_corporate_sector> regulatory_corporate_sector;
};

struct get_many_regulatory_corporate_sectors_request {
    using response_type = struct get_many_regulatory_corporate_sectors_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.regulatory_corporate_sectors.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<regulatory_corporate_sector_key> keys;
};

struct get_many_regulatory_corporate_sectors_response {
    ores::utility::domain::result result;
    std::vector<regulatory_corporate_sector_lookup> entries;
};

struct put_regulatory_corporate_sector_request {
    using response_type = struct put_regulatory_corporate_sector_response;
    static constexpr std::string_view nats_subject = "refdata.v1.regulatory_corporate_sectors.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    regulatory_corporate_sector_change change;
    ores::utility::domain::change_intent intent;
};

struct put_regulatory_corporate_sector_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::regulatory_corporate_sector> regulatory_corporate_sector;
};

struct put_many_regulatory_corporate_sectors_request {
    using response_type = struct put_many_regulatory_corporate_sectors_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.regulatory_corporate_sectors.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<regulatory_corporate_sector_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_regulatory_corporate_sectors_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::regulatory_corporate_sector> sectors;
};

struct delete_regulatory_corporate_sector_request {
    using response_type = struct delete_regulatory_corporate_sector_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.regulatory_corporate_sectors.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    regulatory_corporate_sector_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_regulatory_corporate_sector_response {
    ores::utility::domain::result result;
};

struct delete_many_regulatory_corporate_sectors_request {
    using response_type = struct delete_many_regulatory_corporate_sectors_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.regulatory_corporate_sectors.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<regulatory_corporate_sector_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_regulatory_corporate_sectors_response {
    ores::utility::domain::result result;
};

struct list_regulatory_corporate_sector_versions_request {
    using response_type = struct list_regulatory_corporate_sector_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.regulatory_corporate_sectors_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    regulatory_corporate_sector_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<regulatory_corporate_sector_versions_filter> filter;
};

struct list_regulatory_corporate_sector_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::regulatory_corporate_sector> versions;
    std::uint64_t total;
};

struct get_regulatory_corporate_sector_version_request {
    using response_type = struct get_regulatory_corporate_sector_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.regulatory_corporate_sectors_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    regulatory_corporate_sector_version_key key;
};

struct get_regulatory_corporate_sector_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::regulatory_corporate_sector> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace regulatory_corporate_sector_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.regulatory_corporate_sectors_events.created";
inline constexpr std::string_view updated =
    "refdata.v1.regulatory_corporate_sectors_events.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.regulatory_corporate_sectors_events.deleted";
}
/**
 * @brief The members of regulatory_corporate_sector_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const regulatory_corporate_sector_key&) {
    return std::tuple{
        utility::codec::field{"code", &regulatory_corporate_sector_key::code},
        utility::codec::field{"coding_scheme_code",
                              &regulatory_corporate_sector_key::coding_scheme_code},
    };
}

/**
 * @brief The members of regulatory_corporate_sector_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const regulatory_corporate_sector_write&) {
    return std::tuple{
        utility::codec::field{"code", &regulatory_corporate_sector_write::code},
        utility::codec::field{"coding_scheme_code",
                              &regulatory_corporate_sector_write::coding_scheme_code},
        utility::codec::field{"source", &regulatory_corporate_sector_write::source},
        utility::codec::field{"description", &regulatory_corporate_sector_write::description},
    };
}

/**
 * @brief The members of regulatory_corporate_sector_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const regulatory_corporate_sector_change&) {
    return std::tuple{
        utility::codec::field{"write", &regulatory_corporate_sector_change::write},
        utility::codec::field{"precondition", &regulatory_corporate_sector_change::precondition},
    };
}

/**
 * @brief The members of regulatory_corporate_sector_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const regulatory_corporate_sector_removal&) {
    return std::tuple{
        utility::codec::field{"key", &regulatory_corporate_sector_removal::key},
        utility::codec::field{"precondition", &regulatory_corporate_sector_removal::precondition},
    };
}

/**
 * @brief The members of regulatory_corporate_sector_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const regulatory_corporate_sector_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &regulatory_corporate_sector_lookup::key},
        utility::codec::field{"regulatory_corporate_sector",
                              &regulatory_corporate_sector_lookup::regulatory_corporate_sector},
    };
}

/**
 * @brief The members of regulatory_corporate_sector_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const regulatory_corporate_sector_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &regulatory_corporate_sector_event::event_id},
        utility::codec::field{"key", &regulatory_corporate_sector_event::key},
        utility::codec::field{"action", &regulatory_corporate_sector_event::action},
        utility::codec::field{"version", &regulatory_corporate_sector_event::version},
        utility::codec::field{"occurred_at", &regulatory_corporate_sector_event::occurred_at},
        utility::codec::field{"correlation_id", &regulatory_corporate_sector_event::correlation_id},
    };
}

/**
 * @brief The members of regulatory_corporate_sector_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const regulatory_corporate_sector_version_key&) {
    return std::tuple{
        utility::codec::field{
            "regulatory_corporate_sector",
            &regulatory_corporate_sector_version_key::regulatory_corporate_sector},
        utility::codec::field{"version", &regulatory_corporate_sector_version_key::version},
    };
}

/**
 * @brief The members of regulatory_corporate_sector_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const regulatory_corporate_sector_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &regulatory_corporate_sector_versions_filter::version},
        utility::codec::field{"from_version",
                              &regulatory_corporate_sector_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &regulatory_corporate_sector_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_regulatory_corporate_sectors_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_regulatory_corporate_sectors_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_regulatory_corporate_sectors_request::offset},
        utility::codec::field{"limit", &list_regulatory_corporate_sectors_request::limit},
        utility::codec::field{"order", &list_regulatory_corporate_sectors_request::order},
        utility::codec::field{"as_of", &list_regulatory_corporate_sectors_request::as_of},
    };
}

/**
 * @brief The members of list_regulatory_corporate_sectors_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_regulatory_corporate_sectors_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_regulatory_corporate_sectors_response::result},
        utility::codec::field{"sectors", &list_regulatory_corporate_sectors_response::sectors},
        utility::codec::field{"total", &list_regulatory_corporate_sectors_response::total},
    };
}

/**
 * @brief The members of get_regulatory_corporate_sector_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_regulatory_corporate_sector_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_regulatory_corporate_sector_request::key},
    };
}

/**
 * @brief The members of get_regulatory_corporate_sector_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_regulatory_corporate_sector_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_regulatory_corporate_sector_response::result},
        utility::codec::field{
            "regulatory_corporate_sector",
            &get_regulatory_corporate_sector_response::regulatory_corporate_sector},
    };
}

/**
 * @brief The members of get_many_regulatory_corporate_sectors_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_regulatory_corporate_sectors_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_regulatory_corporate_sectors_request::keys},
    };
}

/**
 * @brief The members of get_many_regulatory_corporate_sectors_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_regulatory_corporate_sectors_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_regulatory_corporate_sectors_response::result},
        utility::codec::field{"entries", &get_many_regulatory_corporate_sectors_response::entries},
    };
}

/**
 * @brief The members of put_regulatory_corporate_sector_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_regulatory_corporate_sector_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_regulatory_corporate_sector_request::change},
        utility::codec::field{"intent", &put_regulatory_corporate_sector_request::intent},
    };
}

/**
 * @brief The members of put_regulatory_corporate_sector_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_regulatory_corporate_sector_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_regulatory_corporate_sector_response::result},
        utility::codec::field{
            "regulatory_corporate_sector",
            &put_regulatory_corporate_sector_response::regulatory_corporate_sector},
    };
}

/**
 * @brief The members of put_many_regulatory_corporate_sectors_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_regulatory_corporate_sectors_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_regulatory_corporate_sectors_request::changes},
        utility::codec::field{"intent", &put_many_regulatory_corporate_sectors_request::intent},
    };
}

/**
 * @brief The members of put_many_regulatory_corporate_sectors_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_regulatory_corporate_sectors_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_regulatory_corporate_sectors_response::result},
        utility::codec::field{"sectors", &put_many_regulatory_corporate_sectors_response::sectors},
    };
}

/**
 * @brief The members of delete_regulatory_corporate_sector_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_regulatory_corporate_sector_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_regulatory_corporate_sector_request::removal},
        utility::codec::field{"intent", &delete_regulatory_corporate_sector_request::intent},
    };
}

/**
 * @brief The members of delete_regulatory_corporate_sector_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_regulatory_corporate_sector_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_regulatory_corporate_sector_response::result},
    };
}

/**
 * @brief The members of delete_many_regulatory_corporate_sectors_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_regulatory_corporate_sectors_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_regulatory_corporate_sectors_request::removals},
        utility::codec::field{"intent", &delete_many_regulatory_corporate_sectors_request::intent},
    };
}

/**
 * @brief The members of delete_many_regulatory_corporate_sectors_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_regulatory_corporate_sectors_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_regulatory_corporate_sectors_response::result},
    };
}

/**
 * @brief The members of list_regulatory_corporate_sector_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_regulatory_corporate_sector_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_regulatory_corporate_sector_versions_request::key},
        utility::codec::field{"offset", &list_regulatory_corporate_sector_versions_request::offset},
        utility::codec::field{"limit", &list_regulatory_corporate_sector_versions_request::limit},
        utility::codec::field{"order", &list_regulatory_corporate_sector_versions_request::order},
        utility::codec::field{"filter", &list_regulatory_corporate_sector_versions_request::filter},
    };
}

/**
 * @brief The members of list_regulatory_corporate_sector_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_regulatory_corporate_sector_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_regulatory_corporate_sector_versions_response::result},
        utility::codec::field{"versions",
                              &list_regulatory_corporate_sector_versions_response::versions},
        utility::codec::field{"total", &list_regulatory_corporate_sector_versions_response::total},
    };
}

/**
 * @brief The members of get_regulatory_corporate_sector_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_regulatory_corporate_sector_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_regulatory_corporate_sector_version_request::key},
    };
}

/**
 * @brief The members of get_regulatory_corporate_sector_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_regulatory_corporate_sector_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_regulatory_corporate_sector_version_response::result},
        utility::codec::field{"version",
                              &get_regulatory_corporate_sector_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::regulatory_corporate_sector_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::regulatory_corporate_sector_key& v);
    static ores::refdata::messaging::regulatory_corporate_sector_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::regulatory_corporate_sector_key& v);
    static ores::refdata::messaging::regulatory_corporate_sector_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::regulatory_corporate_sector_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::regulatory_corporate_sector_write& v);
    static ores::refdata::messaging::regulatory_corporate_sector_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::regulatory_corporate_sector_write& v);
    static ores::refdata::messaging::regulatory_corporate_sector_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::regulatory_corporate_sector_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::regulatory_corporate_sector_change& v);
    static ores::refdata::messaging::regulatory_corporate_sector_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::regulatory_corporate_sector_change& v);
    static ores::refdata::messaging::regulatory_corporate_sector_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::regulatory_corporate_sector_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::regulatory_corporate_sector_removal& v);
    static ores::refdata::messaging::regulatory_corporate_sector_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::regulatory_corporate_sector_removal& v);
    static ores::refdata::messaging::regulatory_corporate_sector_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::regulatory_corporate_sector_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::regulatory_corporate_sector_lookup& v);
    static ores::refdata::messaging::regulatory_corporate_sector_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::regulatory_corporate_sector_lookup& v);
    static ores::refdata::messaging::regulatory_corporate_sector_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::regulatory_corporate_sector_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::regulatory_corporate_sector_event& v);
    static ores::refdata::messaging::regulatory_corporate_sector_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::regulatory_corporate_sector_event& v);
    static ores::refdata::messaging::regulatory_corporate_sector_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::regulatory_corporate_sector_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::regulatory_corporate_sector_version_key& v);
    static ores::refdata::messaging::regulatory_corporate_sector_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::regulatory_corporate_sector_version_key& v);
    static ores::refdata::messaging::regulatory_corporate_sector_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::regulatory_corporate_sector_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::regulatory_corporate_sector_versions_filter& v);
    static ores::refdata::messaging::regulatory_corporate_sector_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::regulatory_corporate_sector_versions_filter& v);
    static ores::refdata::messaging::regulatory_corporate_sector_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_regulatory_corporate_sectors_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_regulatory_corporate_sectors_request& v);
    static ores::refdata::messaging::list_regulatory_corporate_sectors_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_regulatory_corporate_sectors_request& v);
    static ores::refdata::messaging::list_regulatory_corporate_sectors_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_regulatory_corporate_sectors_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_regulatory_corporate_sectors_response& v);
    static ores::refdata::messaging::list_regulatory_corporate_sectors_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_regulatory_corporate_sectors_response& v);
    static ores::refdata::messaging::list_regulatory_corporate_sectors_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_regulatory_corporate_sector_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_regulatory_corporate_sector_request& v);
    static ores::refdata::messaging::get_regulatory_corporate_sector_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_regulatory_corporate_sector_request& v);
    static ores::refdata::messaging::get_regulatory_corporate_sector_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_regulatory_corporate_sector_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_regulatory_corporate_sector_response& v);
    static ores::refdata::messaging::get_regulatory_corporate_sector_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_regulatory_corporate_sector_response& v);
    static ores::refdata::messaging::get_regulatory_corporate_sector_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_regulatory_corporate_sectors_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_regulatory_corporate_sectors_request& v);
    static ores::refdata::messaging::get_many_regulatory_corporate_sectors_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_regulatory_corporate_sectors_request& v);
    static ores::refdata::messaging::get_many_regulatory_corporate_sectors_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_regulatory_corporate_sectors_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_regulatory_corporate_sectors_response& v);
    static ores::refdata::messaging::get_many_regulatory_corporate_sectors_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_regulatory_corporate_sectors_response& v);
    static ores::refdata::messaging::get_many_regulatory_corporate_sectors_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_regulatory_corporate_sector_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_regulatory_corporate_sector_request& v);
    static ores::refdata::messaging::put_regulatory_corporate_sector_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_regulatory_corporate_sector_request& v);
    static ores::refdata::messaging::put_regulatory_corporate_sector_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_regulatory_corporate_sector_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_regulatory_corporate_sector_response& v);
    static ores::refdata::messaging::put_regulatory_corporate_sector_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_regulatory_corporate_sector_response& v);
    static ores::refdata::messaging::put_regulatory_corporate_sector_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_regulatory_corporate_sectors_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_regulatory_corporate_sectors_request& v);
    static ores::refdata::messaging::put_many_regulatory_corporate_sectors_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_regulatory_corporate_sectors_request& v);
    static ores::refdata::messaging::put_many_regulatory_corporate_sectors_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_regulatory_corporate_sectors_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_regulatory_corporate_sectors_response& v);
    static ores::refdata::messaging::put_many_regulatory_corporate_sectors_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_regulatory_corporate_sectors_response& v);
    static ores::refdata::messaging::put_many_regulatory_corporate_sectors_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_regulatory_corporate_sector_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_regulatory_corporate_sector_request& v);
    static ores::refdata::messaging::delete_regulatory_corporate_sector_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_regulatory_corporate_sector_request& v);
    static ores::refdata::messaging::delete_regulatory_corporate_sector_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_regulatory_corporate_sector_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_regulatory_corporate_sector_response& v);
    static ores::refdata::messaging::delete_regulatory_corporate_sector_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_regulatory_corporate_sector_response& v);
    static ores::refdata::messaging::delete_regulatory_corporate_sector_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_regulatory_corporate_sectors_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_regulatory_corporate_sectors_request& v);
    static ores::refdata::messaging::delete_many_regulatory_corporate_sectors_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_regulatory_corporate_sectors_request& v);
    static ores::refdata::messaging::delete_many_regulatory_corporate_sectors_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_regulatory_corporate_sectors_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_regulatory_corporate_sectors_response& v);
    static ores::refdata::messaging::delete_many_regulatory_corporate_sectors_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_regulatory_corporate_sectors_response& v);
    static ores::refdata::messaging::delete_many_regulatory_corporate_sectors_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_regulatory_corporate_sector_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_regulatory_corporate_sector_versions_request& v);
    static ores::refdata::messaging::list_regulatory_corporate_sector_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_regulatory_corporate_sector_versions_request& v);
    static ores::refdata::messaging::list_regulatory_corporate_sector_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_regulatory_corporate_sector_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_regulatory_corporate_sector_versions_response& v);
    static ores::refdata::messaging::list_regulatory_corporate_sector_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_regulatory_corporate_sector_versions_response& v);
    static ores::refdata::messaging::list_regulatory_corporate_sector_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_regulatory_corporate_sector_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_regulatory_corporate_sector_version_request& v);
    static ores::refdata::messaging::get_regulatory_corporate_sector_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_regulatory_corporate_sector_version_request& v);
    static ores::refdata::messaging::get_regulatory_corporate_sector_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_regulatory_corporate_sector_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_regulatory_corporate_sector_version_response& v);
    static ores::refdata::messaging::get_regulatory_corporate_sector_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_regulatory_corporate_sector_version_response& v);
    static ores::refdata::messaging::get_regulatory_corporate_sector_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
