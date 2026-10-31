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
#ifndef ORES_REFDATA_API_MESSAGING_BUSINESS_CENTRE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BUSINESS_CENTRE_PROTOCOL_HPP

#include "ores.refdata.api/domain/business_centre.hpp"
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

struct business_centre_key {
    std::string code;
};

struct business_centre_write {
    std::string code;
    std::string source;
    std::string description;
    std::string city_name;
    std::string country_alpha2_code;
    std::string coding_scheme_code;
};

struct business_centre_change {
    business_centre_write write;
    ores::utility::domain::precondition precondition;
};

struct business_centre_removal {
    business_centre_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct business_centre_lookup {
    business_centre_key key;
    std::optional<ores::refdata::domain::business_centre> business_centre;
};

struct business_centres_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct business_centre_event {
    boost::uuids::uuid event_id;
    business_centre_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct business_centre_version_key {
    business_centre_key business_centre;
    std::uint32_t version;
};

struct business_centre_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_business_centres_request {
    using response_type = struct list_business_centres_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.list";
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
    std::optional<business_centres_filter> filter;
    std::optional<std::string> as_of;
};

struct list_business_centres_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::business_centre> centres;
    std::uint64_t total;
};

struct get_business_centre_request {
    using response_type = struct get_business_centre_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    business_centre_key key;
};

struct get_business_centre_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::business_centre> business_centre;
};

struct get_many_business_centres_request {
    using response_type = struct get_many_business_centres_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<business_centre_key> keys;
};

struct get_many_business_centres_response {
    ores::utility::domain::result result;
    std::vector<business_centre_lookup> entries;
};

struct put_business_centre_request {
    using response_type = struct put_business_centre_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    business_centre_change change;
    ores::utility::domain::change_intent intent;
};

struct put_business_centre_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::business_centre> business_centre;
};

struct put_many_business_centres_request {
    using response_type = struct put_many_business_centres_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<business_centre_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_business_centres_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::business_centre> centres;
};

struct delete_business_centre_request {
    using response_type = struct delete_business_centre_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    business_centre_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_business_centre_response {
    ores::utility::domain::result result;
};

struct delete_many_business_centres_request {
    using response_type = struct delete_many_business_centres_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<business_centre_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_business_centres_response {
    ores::utility::domain::result result;
};

struct list_business_centre_versions_request {
    using response_type = struct list_business_centre_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    business_centre_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<business_centre_versions_filter> filter;
};

struct list_business_centre_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::business_centre> versions;
    std::uint64_t total;
};

struct get_business_centre_version_request {
    using response_type = struct get_business_centre_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.business_centres_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    business_centre_version_key key;
};

struct get_business_centre_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::business_centre> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace business_centre_event_subjects {
inline constexpr std::string_view created = "refdata.v1.business_centres_events.created";
inline constexpr std::string_view updated = "refdata.v1.business_centres_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.business_centres_events.deleted";
}
/**
 * @brief The members of business_centre_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const business_centre_key&) {
    return std::tuple{
        utility::codec::field{"code", &business_centre_key::code},
    };
}

/**
 * @brief The members of business_centre_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const business_centre_write&) {
    return std::tuple{
        utility::codec::field{"code", &business_centre_write::code},
        utility::codec::field{"source", &business_centre_write::source},
        utility::codec::field{"description", &business_centre_write::description},
        utility::codec::field{"city_name", &business_centre_write::city_name},
        utility::codec::field{"country_alpha2_code", &business_centre_write::country_alpha2_code},
        utility::codec::field{"coding_scheme_code", &business_centre_write::coding_scheme_code},
    };
}

/**
 * @brief The members of business_centre_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const business_centre_change&) {
    return std::tuple{
        utility::codec::field{"write", &business_centre_change::write},
        utility::codec::field{"precondition", &business_centre_change::precondition},
    };
}

/**
 * @brief The members of business_centre_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const business_centre_removal&) {
    return std::tuple{
        utility::codec::field{"key", &business_centre_removal::key},
        utility::codec::field{"precondition", &business_centre_removal::precondition},
    };
}

/**
 * @brief The members of business_centre_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const business_centre_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &business_centre_lookup::key},
        utility::codec::field{"business_centre", &business_centre_lookup::business_centre},
    };
}

/**
 * @brief The members of business_centres_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const business_centres_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &business_centres_filter::code_one_of},
    };
}

/**
 * @brief The members of business_centre_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const business_centre_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &business_centre_event::event_id},
        utility::codec::field{"key", &business_centre_event::key},
        utility::codec::field{"action", &business_centre_event::action},
        utility::codec::field{"version", &business_centre_event::version},
        utility::codec::field{"occurred_at", &business_centre_event::occurred_at},
        utility::codec::field{"correlation_id", &business_centre_event::correlation_id},
    };
}

/**
 * @brief The members of business_centre_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const business_centre_version_key&) {
    return std::tuple{
        utility::codec::field{"business_centre", &business_centre_version_key::business_centre},
        utility::codec::field{"version", &business_centre_version_key::version},
    };
}

/**
 * @brief The members of business_centre_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const business_centre_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &business_centre_versions_filter::version},
        utility::codec::field{"from_version", &business_centre_versions_filter::from_version},
        utility::codec::field{"to_version", &business_centre_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_business_centres_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_business_centres_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_business_centres_request::offset},
        utility::codec::field{"limit", &list_business_centres_request::limit},
        utility::codec::field{"order", &list_business_centres_request::order},
        utility::codec::field{"filter", &list_business_centres_request::filter},
        utility::codec::field{"as_of", &list_business_centres_request::as_of},
    };
}

/**
 * @brief The members of list_business_centres_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_business_centres_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_business_centres_response::result},
        utility::codec::field{"centres", &list_business_centres_response::centres},
        utility::codec::field{"total", &list_business_centres_response::total},
    };
}

/**
 * @brief The members of get_business_centre_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_business_centre_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_business_centre_request::key},
    };
}

/**
 * @brief The members of get_business_centre_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_business_centre_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_business_centre_response::result},
        utility::codec::field{"business_centre", &get_business_centre_response::business_centre},
    };
}

/**
 * @brief The members of get_many_business_centres_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_business_centres_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_business_centres_request::keys},
    };
}

/**
 * @brief The members of get_many_business_centres_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_business_centres_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_business_centres_response::result},
        utility::codec::field{"entries", &get_many_business_centres_response::entries},
    };
}

/**
 * @brief The members of put_business_centre_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_business_centre_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_business_centre_request::change},
        utility::codec::field{"intent", &put_business_centre_request::intent},
    };
}

/**
 * @brief The members of put_business_centre_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_business_centre_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_business_centre_response::result},
        utility::codec::field{"business_centre", &put_business_centre_response::business_centre},
    };
}

/**
 * @brief The members of put_many_business_centres_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_business_centres_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_business_centres_request::changes},
        utility::codec::field{"intent", &put_many_business_centres_request::intent},
    };
}

/**
 * @brief The members of put_many_business_centres_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_business_centres_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_business_centres_response::result},
        utility::codec::field{"centres", &put_many_business_centres_response::centres},
    };
}

/**
 * @brief The members of delete_business_centre_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_business_centre_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_business_centre_request::removal},
        utility::codec::field{"intent", &delete_business_centre_request::intent},
    };
}

/**
 * @brief The members of delete_business_centre_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_business_centre_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_business_centre_response::result},
    };
}

/**
 * @brief The members of delete_many_business_centres_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_business_centres_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_business_centres_request::removals},
        utility::codec::field{"intent", &delete_many_business_centres_request::intent},
    };
}

/**
 * @brief The members of delete_many_business_centres_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_business_centres_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_business_centres_response::result},
    };
}

/**
 * @brief The members of list_business_centre_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_business_centre_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_business_centre_versions_request::key},
        utility::codec::field{"offset", &list_business_centre_versions_request::offset},
        utility::codec::field{"limit", &list_business_centre_versions_request::limit},
        utility::codec::field{"order", &list_business_centre_versions_request::order},
        utility::codec::field{"filter", &list_business_centre_versions_request::filter},
    };
}

/**
 * @brief The members of list_business_centre_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_business_centre_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_business_centre_versions_response::result},
        utility::codec::field{"versions", &list_business_centre_versions_response::versions},
        utility::codec::field{"total", &list_business_centre_versions_response::total},
    };
}

/**
 * @brief The members of get_business_centre_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_business_centre_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_business_centre_version_request::key},
    };
}

/**
 * @brief The members of get_business_centre_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_business_centre_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_business_centre_version_response::result},
        utility::codec::field{"version", &get_business_centre_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::business_centre_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::business_centre_key& v);
    static ores::refdata::messaging::business_centre_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::business_centre_key& v);
    static ores::refdata::messaging::business_centre_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::business_centre_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::business_centre_write& v);
    static ores::refdata::messaging::business_centre_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::business_centre_write& v);
    static ores::refdata::messaging::business_centre_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::business_centre_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::business_centre_change& v);
    static ores::refdata::messaging::business_centre_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::business_centre_change& v);
    static ores::refdata::messaging::business_centre_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::business_centre_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::business_centre_removal& v);
    static ores::refdata::messaging::business_centre_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::business_centre_removal& v);
    static ores::refdata::messaging::business_centre_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::business_centre_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::business_centre_lookup& v);
    static ores::refdata::messaging::business_centre_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::business_centre_lookup& v);
    static ores::refdata::messaging::business_centre_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::business_centres_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::business_centres_filter& v);
    static ores::refdata::messaging::business_centres_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::business_centres_filter& v);
    static ores::refdata::messaging::business_centres_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::business_centre_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::business_centre_event& v);
    static ores::refdata::messaging::business_centre_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::business_centre_event& v);
    static ores::refdata::messaging::business_centre_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::business_centre_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::business_centre_version_key& v);
    static ores::refdata::messaging::business_centre_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::business_centre_version_key& v);
    static ores::refdata::messaging::business_centre_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::business_centre_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::business_centre_versions_filter& v);
    static ores::refdata::messaging::business_centre_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::business_centre_versions_filter& v);
    static ores::refdata::messaging::business_centre_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_business_centres_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_business_centres_request& v);
    static ores::refdata::messaging::list_business_centres_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_business_centres_request& v);
    static ores::refdata::messaging::list_business_centres_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_business_centres_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_business_centres_response& v);
    static ores::refdata::messaging::list_business_centres_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_business_centres_response& v);
    static ores::refdata::messaging::list_business_centres_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_business_centre_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_business_centre_request& v);
    static ores::refdata::messaging::get_business_centre_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_business_centre_request& v);
    static ores::refdata::messaging::get_business_centre_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_business_centre_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_business_centre_response& v);
    static ores::refdata::messaging::get_business_centre_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_business_centre_response& v);
    static ores::refdata::messaging::get_business_centre_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_business_centres_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_business_centres_request& v);
    static ores::refdata::messaging::get_many_business_centres_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_business_centres_request& v);
    static ores::refdata::messaging::get_many_business_centres_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_business_centres_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_business_centres_response& v);
    static ores::refdata::messaging::get_many_business_centres_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_business_centres_response& v);
    static ores::refdata::messaging::get_many_business_centres_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_business_centre_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_business_centre_request& v);
    static ores::refdata::messaging::put_business_centre_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_business_centre_request& v);
    static ores::refdata::messaging::put_business_centre_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_business_centre_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_business_centre_response& v);
    static ores::refdata::messaging::put_business_centre_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_business_centre_response& v);
    static ores::refdata::messaging::put_business_centre_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_business_centres_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_business_centres_request& v);
    static ores::refdata::messaging::put_many_business_centres_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_business_centres_request& v);
    static ores::refdata::messaging::put_many_business_centres_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_business_centres_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_business_centres_response& v);
    static ores::refdata::messaging::put_many_business_centres_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_business_centres_response& v);
    static ores::refdata::messaging::put_many_business_centres_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_business_centre_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_business_centre_request& v);
    static ores::refdata::messaging::delete_business_centre_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_business_centre_request& v);
    static ores::refdata::messaging::delete_business_centre_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_business_centre_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_business_centre_response& v);
    static ores::refdata::messaging::delete_business_centre_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_business_centre_response& v);
    static ores::refdata::messaging::delete_business_centre_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_business_centres_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_business_centres_request& v);
    static ores::refdata::messaging::delete_many_business_centres_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_business_centres_request& v);
    static ores::refdata::messaging::delete_many_business_centres_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_business_centres_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_business_centres_response& v);
    static ores::refdata::messaging::delete_many_business_centres_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_business_centres_response& v);
    static ores::refdata::messaging::delete_many_business_centres_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_business_centre_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_business_centre_versions_request& v);
    static ores::refdata::messaging::list_business_centre_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_business_centre_versions_request& v);
    static ores::refdata::messaging::list_business_centre_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_business_centre_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_business_centre_versions_response& v);
    static ores::refdata::messaging::list_business_centre_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_business_centre_versions_response& v);
    static ores::refdata::messaging::list_business_centre_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_business_centre_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_business_centre_version_request& v);
    static ores::refdata::messaging::get_business_centre_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_business_centre_version_request& v);
    static ores::refdata::messaging::get_business_centre_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_business_centre_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_business_centre_version_response& v);
    static ores::refdata::messaging::get_business_centre_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_business_centre_version_response& v);
    static ores::refdata::messaging::get_business_centre_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
