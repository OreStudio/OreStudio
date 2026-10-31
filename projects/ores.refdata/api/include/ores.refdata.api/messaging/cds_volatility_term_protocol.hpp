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
#ifndef ORES_REFDATA_API_MESSAGING_CDS_VOLATILITY_TERM_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CDS_VOLATILITY_TERM_PROTOCOL_HPP

#include "ores.refdata.api/domain/cds_volatility_term.hpp"
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

struct cds_volatility_term_key {
    boost::uuids::uuid id;
};

struct cds_volatility_term_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_definition_id;
    std::string label;
    std::string curve;
    std::optional<std::string> maturity;
    int position;
};

struct cds_volatility_term_change {
    cds_volatility_term_write write;
    ores::utility::domain::precondition precondition;
};

struct cds_volatility_term_removal {
    cds_volatility_term_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct cds_volatility_term_lookup {
    cds_volatility_term_key key;
    std::optional<ores::refdata::domain::cds_volatility_term> cds_volatility_term;
};

struct cds_volatility_terms_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct cds_volatility_term_event {
    boost::uuids::uuid event_id;
    cds_volatility_term_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct cds_volatility_term_version_key {
    cds_volatility_term_key cds_volatility_term;
    std::uint32_t version;
};

struct cds_volatility_term_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_cds_volatility_terms_request {
    using response_type = struct list_cds_volatility_terms_response;
    static constexpr std::string_view nats_subject = "refdata.v1.cds_volatility_terms.list";
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
    std::optional<cds_volatility_terms_filter> filter;
    std::optional<std::string> as_of;
};

struct list_cds_volatility_terms_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::cds_volatility_term> terms;
    std::uint64_t total;
};

struct get_cds_volatility_term_request {
    using response_type = struct get_cds_volatility_term_response;
    static constexpr std::string_view nats_subject = "refdata.v1.cds_volatility_terms.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cds_volatility_term_key key;
};

struct get_cds_volatility_term_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::cds_volatility_term> cds_volatility_term;
};

struct get_many_cds_volatility_terms_request {
    using response_type = struct get_many_cds_volatility_terms_response;
    static constexpr std::string_view nats_subject = "refdata.v1.cds_volatility_terms.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cds_volatility_term_key> keys;
};

struct get_many_cds_volatility_terms_response {
    ores::utility::domain::result result;
    std::vector<cds_volatility_term_lookup> entries;
};

struct put_cds_volatility_term_request {
    using response_type = struct put_cds_volatility_term_response;
    static constexpr std::string_view nats_subject = "refdata.v1.cds_volatility_terms.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cds_volatility_term_change change;
    ores::utility::domain::change_intent intent;
};

struct put_cds_volatility_term_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::cds_volatility_term> cds_volatility_term;
};

struct put_many_cds_volatility_terms_request {
    using response_type = struct put_many_cds_volatility_terms_response;
    static constexpr std::string_view nats_subject = "refdata.v1.cds_volatility_terms.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cds_volatility_term_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_cds_volatility_terms_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::cds_volatility_term> terms;
};

struct delete_cds_volatility_term_request {
    using response_type = struct delete_cds_volatility_term_response;
    static constexpr std::string_view nats_subject = "refdata.v1.cds_volatility_terms.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cds_volatility_term_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_cds_volatility_term_response {
    ores::utility::domain::result result;
};

struct delete_many_cds_volatility_terms_request {
    using response_type = struct delete_many_cds_volatility_terms_response;
    static constexpr std::string_view nats_subject = "refdata.v1.cds_volatility_terms.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cds_volatility_term_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_cds_volatility_terms_response {
    ores::utility::domain::result result;
};

struct list_cds_volatility_term_versions_request {
    using response_type = struct list_cds_volatility_term_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cds_volatility_terms_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cds_volatility_term_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cds_volatility_term_versions_filter> filter;
};

struct list_cds_volatility_term_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::cds_volatility_term> versions;
    std::uint64_t total;
};

struct get_cds_volatility_term_version_request {
    using response_type = struct get_cds_volatility_term_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.cds_volatility_terms_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cds_volatility_term_version_key key;
};

struct get_cds_volatility_term_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::cds_volatility_term> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace cds_volatility_term_event_subjects {
inline constexpr std::string_view created = "refdata.v1.cds_volatility_terms_events.created";
inline constexpr std::string_view updated = "refdata.v1.cds_volatility_terms_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.cds_volatility_terms_events.deleted";
}
/**
 * @brief The members of cds_volatility_term_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cds_volatility_term_key&) {
    return std::tuple{
        utility::codec::field{"id", &cds_volatility_term_key::id},
    };
}

/**
 * @brief The members of cds_volatility_term_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cds_volatility_term_write&) {
    return std::tuple{
        utility::codec::field{"id", &cds_volatility_term_write::id},
        utility::codec::field{"curve_definition_id",
                              &cds_volatility_term_write::curve_definition_id},
        utility::codec::field{"label", &cds_volatility_term_write::label},
        utility::codec::field{"curve", &cds_volatility_term_write::curve},
        utility::codec::field{"maturity", &cds_volatility_term_write::maturity},
        utility::codec::field{"position", &cds_volatility_term_write::position},
    };
}

/**
 * @brief The members of cds_volatility_term_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cds_volatility_term_change&) {
    return std::tuple{
        utility::codec::field{"write", &cds_volatility_term_change::write},
        utility::codec::field{"precondition", &cds_volatility_term_change::precondition},
    };
}

/**
 * @brief The members of cds_volatility_term_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cds_volatility_term_removal&) {
    return std::tuple{
        utility::codec::field{"key", &cds_volatility_term_removal::key},
        utility::codec::field{"precondition", &cds_volatility_term_removal::precondition},
    };
}

/**
 * @brief The members of cds_volatility_term_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cds_volatility_term_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &cds_volatility_term_lookup::key},
        utility::codec::field{"cds_volatility_term",
                              &cds_volatility_term_lookup::cds_volatility_term},
    };
}

/**
 * @brief The members of cds_volatility_terms_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cds_volatility_terms_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &cds_volatility_terms_filter::id_one_of},
    };
}

/**
 * @brief The members of cds_volatility_term_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cds_volatility_term_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &cds_volatility_term_event::event_id},
        utility::codec::field{"key", &cds_volatility_term_event::key},
        utility::codec::field{"action", &cds_volatility_term_event::action},
        utility::codec::field{"version", &cds_volatility_term_event::version},
        utility::codec::field{"occurred_at", &cds_volatility_term_event::occurred_at},
        utility::codec::field{"correlation_id", &cds_volatility_term_event::correlation_id},
    };
}

/**
 * @brief The members of cds_volatility_term_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cds_volatility_term_version_key&) {
    return std::tuple{
        utility::codec::field{"cds_volatility_term",
                              &cds_volatility_term_version_key::cds_volatility_term},
        utility::codec::field{"version", &cds_volatility_term_version_key::version},
    };
}

/**
 * @brief The members of cds_volatility_term_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cds_volatility_term_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &cds_volatility_term_versions_filter::version},
        utility::codec::field{"from_version", &cds_volatility_term_versions_filter::from_version},
        utility::codec::field{"to_version", &cds_volatility_term_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_cds_volatility_terms_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_cds_volatility_terms_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_cds_volatility_terms_request::offset},
        utility::codec::field{"limit", &list_cds_volatility_terms_request::limit},
        utility::codec::field{"order", &list_cds_volatility_terms_request::order},
        utility::codec::field{"filter", &list_cds_volatility_terms_request::filter},
        utility::codec::field{"as_of", &list_cds_volatility_terms_request::as_of},
    };
}

/**
 * @brief The members of list_cds_volatility_terms_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_cds_volatility_terms_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_cds_volatility_terms_response::result},
        utility::codec::field{"terms", &list_cds_volatility_terms_response::terms},
        utility::codec::field{"total", &list_cds_volatility_terms_response::total},
    };
}

/**
 * @brief The members of get_cds_volatility_term_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_cds_volatility_term_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_cds_volatility_term_request::key},
    };
}

/**
 * @brief The members of get_cds_volatility_term_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_cds_volatility_term_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_cds_volatility_term_response::result},
        utility::codec::field{"cds_volatility_term",
                              &get_cds_volatility_term_response::cds_volatility_term},
    };
}

/**
 * @brief The members of get_many_cds_volatility_terms_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_cds_volatility_terms_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_cds_volatility_terms_request::keys},
    };
}

/**
 * @brief The members of get_many_cds_volatility_terms_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_cds_volatility_terms_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_cds_volatility_terms_response::result},
        utility::codec::field{"entries", &get_many_cds_volatility_terms_response::entries},
    };
}

/**
 * @brief The members of put_cds_volatility_term_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_cds_volatility_term_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_cds_volatility_term_request::change},
        utility::codec::field{"intent", &put_cds_volatility_term_request::intent},
    };
}

/**
 * @brief The members of put_cds_volatility_term_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_cds_volatility_term_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_cds_volatility_term_response::result},
        utility::codec::field{"cds_volatility_term",
                              &put_cds_volatility_term_response::cds_volatility_term},
    };
}

/**
 * @brief The members of put_many_cds_volatility_terms_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_cds_volatility_terms_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_cds_volatility_terms_request::changes},
        utility::codec::field{"intent", &put_many_cds_volatility_terms_request::intent},
    };
}

/**
 * @brief The members of put_many_cds_volatility_terms_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_cds_volatility_terms_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_cds_volatility_terms_response::result},
        utility::codec::field{"terms", &put_many_cds_volatility_terms_response::terms},
    };
}

/**
 * @brief The members of delete_cds_volatility_term_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_cds_volatility_term_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_cds_volatility_term_request::removal},
        utility::codec::field{"intent", &delete_cds_volatility_term_request::intent},
    };
}

/**
 * @brief The members of delete_cds_volatility_term_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_cds_volatility_term_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_cds_volatility_term_response::result},
    };
}

/**
 * @brief The members of delete_many_cds_volatility_terms_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_cds_volatility_terms_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_cds_volatility_terms_request::removals},
        utility::codec::field{"intent", &delete_many_cds_volatility_terms_request::intent},
    };
}

/**
 * @brief The members of delete_many_cds_volatility_terms_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_cds_volatility_terms_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_cds_volatility_terms_response::result},
    };
}

/**
 * @brief The members of list_cds_volatility_term_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_cds_volatility_term_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_cds_volatility_term_versions_request::key},
        utility::codec::field{"offset", &list_cds_volatility_term_versions_request::offset},
        utility::codec::field{"limit", &list_cds_volatility_term_versions_request::limit},
        utility::codec::field{"order", &list_cds_volatility_term_versions_request::order},
        utility::codec::field{"filter", &list_cds_volatility_term_versions_request::filter},
    };
}

/**
 * @brief The members of list_cds_volatility_term_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_cds_volatility_term_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_cds_volatility_term_versions_response::result},
        utility::codec::field{"versions", &list_cds_volatility_term_versions_response::versions},
        utility::codec::field{"total", &list_cds_volatility_term_versions_response::total},
    };
}

/**
 * @brief The members of get_cds_volatility_term_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_cds_volatility_term_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_cds_volatility_term_version_request::key},
    };
}

/**
 * @brief The members of get_cds_volatility_term_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_cds_volatility_term_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_cds_volatility_term_version_response::result},
        utility::codec::field{"version", &get_cds_volatility_term_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::cds_volatility_term_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::cds_volatility_term_key& v);
    static ores::refdata::messaging::cds_volatility_term_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::cds_volatility_term_key& v);
    static ores::refdata::messaging::cds_volatility_term_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::cds_volatility_term_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::cds_volatility_term_write& v);
    static ores::refdata::messaging::cds_volatility_term_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::cds_volatility_term_write& v);
    static ores::refdata::messaging::cds_volatility_term_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::cds_volatility_term_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::cds_volatility_term_change& v);
    static ores::refdata::messaging::cds_volatility_term_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::cds_volatility_term_change& v);
    static ores::refdata::messaging::cds_volatility_term_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::cds_volatility_term_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::cds_volatility_term_removal& v);
    static ores::refdata::messaging::cds_volatility_term_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cds_volatility_term_removal& v);
    static ores::refdata::messaging::cds_volatility_term_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::cds_volatility_term_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::cds_volatility_term_lookup& v);
    static ores::refdata::messaging::cds_volatility_term_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::cds_volatility_term_lookup& v);
    static ores::refdata::messaging::cds_volatility_term_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::cds_volatility_terms_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::cds_volatility_terms_filter& v);
    static ores::refdata::messaging::cds_volatility_terms_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cds_volatility_terms_filter& v);
    static ores::refdata::messaging::cds_volatility_terms_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::cds_volatility_term_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::cds_volatility_term_event& v);
    static ores::refdata::messaging::cds_volatility_term_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::cds_volatility_term_event& v);
    static ores::refdata::messaging::cds_volatility_term_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cds_volatility_term_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cds_volatility_term_version_key& v);
    static ores::refdata::messaging::cds_volatility_term_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cds_volatility_term_version_key& v);
    static ores::refdata::messaging::cds_volatility_term_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cds_volatility_term_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cds_volatility_term_versions_filter& v);
    static ores::refdata::messaging::cds_volatility_term_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cds_volatility_term_versions_filter& v);
    static ores::refdata::messaging::cds_volatility_term_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_cds_volatility_terms_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_cds_volatility_terms_request& v);
    static ores::refdata::messaging::list_cds_volatility_terms_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_cds_volatility_terms_request& v);
    static ores::refdata::messaging::list_cds_volatility_terms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_cds_volatility_terms_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_cds_volatility_terms_response& v);
    static ores::refdata::messaging::list_cds_volatility_terms_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_cds_volatility_terms_response& v);
    static ores::refdata::messaging::list_cds_volatility_terms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_cds_volatility_term_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_cds_volatility_term_request& v);
    static ores::refdata::messaging::get_cds_volatility_term_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_cds_volatility_term_request& v);
    static ores::refdata::messaging::get_cds_volatility_term_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_cds_volatility_term_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_cds_volatility_term_response& v);
    static ores::refdata::messaging::get_cds_volatility_term_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_cds_volatility_term_response& v);
    static ores::refdata::messaging::get_cds_volatility_term_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_cds_volatility_terms_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_cds_volatility_terms_request& v);
    static ores::refdata::messaging::get_many_cds_volatility_terms_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_cds_volatility_terms_request& v);
    static ores::refdata::messaging::get_many_cds_volatility_terms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_cds_volatility_terms_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_cds_volatility_terms_response& v);
    static ores::refdata::messaging::get_many_cds_volatility_terms_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_cds_volatility_terms_response& v);
    static ores::refdata::messaging::get_many_cds_volatility_terms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_cds_volatility_term_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_cds_volatility_term_request& v);
    static ores::refdata::messaging::put_cds_volatility_term_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_cds_volatility_term_request& v);
    static ores::refdata::messaging::put_cds_volatility_term_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_cds_volatility_term_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_cds_volatility_term_response& v);
    static ores::refdata::messaging::put_cds_volatility_term_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_cds_volatility_term_response& v);
    static ores::refdata::messaging::put_cds_volatility_term_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_cds_volatility_terms_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_cds_volatility_terms_request& v);
    static ores::refdata::messaging::put_many_cds_volatility_terms_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_cds_volatility_terms_request& v);
    static ores::refdata::messaging::put_many_cds_volatility_terms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_cds_volatility_terms_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_cds_volatility_terms_response& v);
    static ores::refdata::messaging::put_many_cds_volatility_terms_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_cds_volatility_terms_response& v);
    static ores::refdata::messaging::put_many_cds_volatility_terms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_cds_volatility_term_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_cds_volatility_term_request& v);
    static ores::refdata::messaging::delete_cds_volatility_term_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_cds_volatility_term_request& v);
    static ores::refdata::messaging::delete_cds_volatility_term_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_cds_volatility_term_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_cds_volatility_term_response& v);
    static ores::refdata::messaging::delete_cds_volatility_term_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_cds_volatility_term_response& v);
    static ores::refdata::messaging::delete_cds_volatility_term_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_cds_volatility_terms_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_cds_volatility_terms_request& v);
    static ores::refdata::messaging::delete_many_cds_volatility_terms_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_cds_volatility_terms_request& v);
    static ores::refdata::messaging::delete_many_cds_volatility_terms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_cds_volatility_terms_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_cds_volatility_terms_response& v);
    static ores::refdata::messaging::delete_many_cds_volatility_terms_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_cds_volatility_terms_response& v);
    static ores::refdata::messaging::delete_many_cds_volatility_terms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_cds_volatility_term_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_cds_volatility_term_versions_request& v);
    static ores::refdata::messaging::list_cds_volatility_term_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_cds_volatility_term_versions_request& v);
    static ores::refdata::messaging::list_cds_volatility_term_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_cds_volatility_term_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_cds_volatility_term_versions_response& v);
    static ores::refdata::messaging::list_cds_volatility_term_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_cds_volatility_term_versions_response& v);
    static ores::refdata::messaging::list_cds_volatility_term_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_cds_volatility_term_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_cds_volatility_term_version_request& v);
    static ores::refdata::messaging::get_cds_volatility_term_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_cds_volatility_term_version_request& v);
    static ores::refdata::messaging::get_cds_volatility_term_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_cds_volatility_term_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_cds_volatility_term_version_response& v);
    static ores::refdata::messaging::get_cds_volatility_term_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_cds_volatility_term_version_response& v);
    static ores::refdata::messaging::get_cds_volatility_term_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
