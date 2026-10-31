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
#ifndef ORES_REFDATA_API_MESSAGING_NETTING_AGREEMENT_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_NETTING_AGREEMENT_PROTOCOL_HPP

#include "ores.refdata.api/domain/netting_agreement.hpp"
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

struct netting_agreement_key {
    std::string agreement_number;
};

struct netting_agreement_write {
    boost::uuids::uuid id;
    std::string agreement_number;
    boost::uuids::uuid party_id;
    boost::uuids::uuid counterparty_id;
    std::string agreement_type;
    std::optional<std::string> governing_law;
    std::optional<std::string> description;
};

struct netting_agreement_change {
    netting_agreement_write write;
    ores::utility::domain::precondition precondition;
};

struct netting_agreement_removal {
    netting_agreement_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct netting_agreement_lookup {
    netting_agreement_key key;
    std::optional<ores::refdata::domain::netting_agreement> netting_agreement;
};

struct netting_agreements_filter {
    std::optional<boost::uuids::uuid> counterparty_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> counterparty_id_one_of;
};

struct netting_agreement_event {
    boost::uuids::uuid event_id;
    netting_agreement_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct netting_agreement_version_key {
    netting_agreement_key netting_agreement;
    std::uint32_t version;
};

struct netting_agreement_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_netting_agreements_request {
    using response_type = struct list_netting_agreements_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.list";
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
    std::optional<netting_agreements_filter> filter;
    std::optional<std::string> as_of;
};

struct list_netting_agreements_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::netting_agreement> netting_agreements;
    std::uint64_t total;
};

struct get_netting_agreement_request {
    using response_type = struct get_netting_agreement_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    netting_agreement_key key;
};

struct get_netting_agreement_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::netting_agreement> netting_agreement;
};

struct get_many_netting_agreements_request {
    using response_type = struct get_many_netting_agreements_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<netting_agreement_key> keys;
};

struct get_many_netting_agreements_response {
    ores::utility::domain::result result;
    std::vector<netting_agreement_lookup> entries;
};

struct put_netting_agreement_request {
    using response_type = struct put_netting_agreement_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    netting_agreement_change change;
    ores::utility::domain::change_intent intent;
};

struct put_netting_agreement_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::netting_agreement> netting_agreement;
};

struct put_many_netting_agreements_request {
    using response_type = struct put_many_netting_agreements_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<netting_agreement_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_netting_agreements_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::netting_agreement> netting_agreements;
};

struct delete_netting_agreement_request {
    using response_type = struct delete_netting_agreement_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    netting_agreement_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_netting_agreement_response {
    ores::utility::domain::result result;
};

struct delete_many_netting_agreements_request {
    using response_type = struct delete_many_netting_agreements_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<netting_agreement_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_netting_agreements_response {
    ores::utility::domain::result result;
};

struct list_by_counterparty_id_netting_agreements_request {
    using response_type = struct list_by_counterparty_id_netting_agreements_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.netting_agreements.list_by_counterparty_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid counterparty_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<netting_agreements_filter> filter;
};

struct list_by_counterparty_id_netting_agreements_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::netting_agreement> netting_agreements;
    std::uint64_t total;
};

struct list_netting_agreement_versions_request {
    using response_type = struct list_netting_agreement_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    netting_agreement_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<netting_agreement_versions_filter> filter;
};

struct list_netting_agreement_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::netting_agreement> versions;
    std::uint64_t total;
};

struct get_netting_agreement_version_request {
    using response_type = struct get_netting_agreement_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    netting_agreement_version_key key;
};

struct get_netting_agreement_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::netting_agreement> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace netting_agreement_event_subjects {
inline constexpr std::string_view created = "refdata.v1.netting_agreements_events.created";
inline constexpr std::string_view updated = "refdata.v1.netting_agreements_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.netting_agreements_events.deleted";
}
/**
 * @brief The members of netting_agreement_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const netting_agreement_key&) {
    return std::tuple{
        utility::codec::field{"agreement_number", &netting_agreement_key::agreement_number},
    };
}

/**
 * @brief The members of netting_agreement_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const netting_agreement_write&) {
    return std::tuple{
        utility::codec::field{"id", &netting_agreement_write::id},
        utility::codec::field{"agreement_number", &netting_agreement_write::agreement_number},
        utility::codec::field{"party_id", &netting_agreement_write::party_id},
        utility::codec::field{"counterparty_id", &netting_agreement_write::counterparty_id},
        utility::codec::field{"agreement_type", &netting_agreement_write::agreement_type},
        utility::codec::field{"governing_law", &netting_agreement_write::governing_law},
        utility::codec::field{"description", &netting_agreement_write::description},
    };
}

/**
 * @brief The members of netting_agreement_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const netting_agreement_change&) {
    return std::tuple{
        utility::codec::field{"write", &netting_agreement_change::write},
        utility::codec::field{"precondition", &netting_agreement_change::precondition},
    };
}

/**
 * @brief The members of netting_agreement_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const netting_agreement_removal&) {
    return std::tuple{
        utility::codec::field{"key", &netting_agreement_removal::key},
        utility::codec::field{"precondition", &netting_agreement_removal::precondition},
    };
}

/**
 * @brief The members of netting_agreement_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const netting_agreement_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &netting_agreement_lookup::key},
        utility::codec::field{"netting_agreement", &netting_agreement_lookup::netting_agreement},
    };
}

/**
 * @brief The members of netting_agreements_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const netting_agreements_filter&) {
    return std::tuple{
        utility::codec::field{"counterparty_id", &netting_agreements_filter::counterparty_id},
        utility::codec::field{"id_one_of", &netting_agreements_filter::id_one_of},
        utility::codec::field{"counterparty_id_one_of",
                              &netting_agreements_filter::counterparty_id_one_of},
    };
}

/**
 * @brief The members of netting_agreement_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const netting_agreement_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &netting_agreement_event::event_id},
        utility::codec::field{"key", &netting_agreement_event::key},
        utility::codec::field{"action", &netting_agreement_event::action},
        utility::codec::field{"version", &netting_agreement_event::version},
        utility::codec::field{"occurred_at", &netting_agreement_event::occurred_at},
        utility::codec::field{"correlation_id", &netting_agreement_event::correlation_id},
    };
}

/**
 * @brief The members of netting_agreement_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const netting_agreement_version_key&) {
    return std::tuple{
        utility::codec::field{"netting_agreement",
                              &netting_agreement_version_key::netting_agreement},
        utility::codec::field{"version", &netting_agreement_version_key::version},
    };
}

/**
 * @brief The members of netting_agreement_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const netting_agreement_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &netting_agreement_versions_filter::version},
        utility::codec::field{"from_version", &netting_agreement_versions_filter::from_version},
        utility::codec::field{"to_version", &netting_agreement_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_netting_agreements_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_netting_agreements_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_netting_agreements_request::offset},
        utility::codec::field{"limit", &list_netting_agreements_request::limit},
        utility::codec::field{"order", &list_netting_agreements_request::order},
        utility::codec::field{"filter", &list_netting_agreements_request::filter},
        utility::codec::field{"as_of", &list_netting_agreements_request::as_of},
    };
}

/**
 * @brief The members of list_netting_agreements_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_netting_agreements_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_netting_agreements_response::result},
        utility::codec::field{"netting_agreements",
                              &list_netting_agreements_response::netting_agreements},
        utility::codec::field{"total", &list_netting_agreements_response::total},
    };
}

/**
 * @brief The members of get_netting_agreement_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_netting_agreement_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_netting_agreement_request::key},
    };
}

/**
 * @brief The members of get_netting_agreement_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_netting_agreement_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_netting_agreement_response::result},
        utility::codec::field{"netting_agreement",
                              &get_netting_agreement_response::netting_agreement},
    };
}

/**
 * @brief The members of get_many_netting_agreements_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_netting_agreements_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_netting_agreements_request::keys},
    };
}

/**
 * @brief The members of get_many_netting_agreements_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_netting_agreements_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_netting_agreements_response::result},
        utility::codec::field{"entries", &get_many_netting_agreements_response::entries},
    };
}

/**
 * @brief The members of put_netting_agreement_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_netting_agreement_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_netting_agreement_request::change},
        utility::codec::field{"intent", &put_netting_agreement_request::intent},
    };
}

/**
 * @brief The members of put_netting_agreement_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_netting_agreement_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_netting_agreement_response::result},
        utility::codec::field{"netting_agreement",
                              &put_netting_agreement_response::netting_agreement},
    };
}

/**
 * @brief The members of put_many_netting_agreements_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_netting_agreements_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_netting_agreements_request::changes},
        utility::codec::field{"intent", &put_many_netting_agreements_request::intent},
    };
}

/**
 * @brief The members of put_many_netting_agreements_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_netting_agreements_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_netting_agreements_response::result},
        utility::codec::field{"netting_agreements",
                              &put_many_netting_agreements_response::netting_agreements},
    };
}

/**
 * @brief The members of delete_netting_agreement_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_netting_agreement_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_netting_agreement_request::removal},
        utility::codec::field{"intent", &delete_netting_agreement_request::intent},
    };
}

/**
 * @brief The members of delete_netting_agreement_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_netting_agreement_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_netting_agreement_response::result},
    };
}

/**
 * @brief The members of delete_many_netting_agreements_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_netting_agreements_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_netting_agreements_request::removals},
        utility::codec::field{"intent", &delete_many_netting_agreements_request::intent},
    };
}

/**
 * @brief The members of delete_many_netting_agreements_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_netting_agreements_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_netting_agreements_response::result},
    };
}

/**
 * @brief The members of list_by_counterparty_id_netting_agreements_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_counterparty_id_netting_agreements_request&) {
    return std::tuple{
        utility::codec::field{"counterparty_id",
                              &list_by_counterparty_id_netting_agreements_request::counterparty_id},
        utility::codec::field{"scope", &list_by_counterparty_id_netting_agreements_request::scope},
        utility::codec::field{"offset",
                              &list_by_counterparty_id_netting_agreements_request::offset},
        utility::codec::field{"limit", &list_by_counterparty_id_netting_agreements_request::limit},
        utility::codec::field{"order", &list_by_counterparty_id_netting_agreements_request::order},
        utility::codec::field{"filter",
                              &list_by_counterparty_id_netting_agreements_request::filter},
    };
}

/**
 * @brief The members of list_by_counterparty_id_netting_agreements_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_counterparty_id_netting_agreements_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_counterparty_id_netting_agreements_response::result},
        utility::codec::field{
            "netting_agreements",
            &list_by_counterparty_id_netting_agreements_response::netting_agreements},
        utility::codec::field{"total", &list_by_counterparty_id_netting_agreements_response::total},
    };
}

/**
 * @brief The members of list_netting_agreement_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_netting_agreement_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_netting_agreement_versions_request::key},
        utility::codec::field{"offset", &list_netting_agreement_versions_request::offset},
        utility::codec::field{"limit", &list_netting_agreement_versions_request::limit},
        utility::codec::field{"order", &list_netting_agreement_versions_request::order},
        utility::codec::field{"filter", &list_netting_agreement_versions_request::filter},
    };
}

/**
 * @brief The members of list_netting_agreement_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_netting_agreement_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_netting_agreement_versions_response::result},
        utility::codec::field{"versions", &list_netting_agreement_versions_response::versions},
        utility::codec::field{"total", &list_netting_agreement_versions_response::total},
    };
}

/**
 * @brief The members of get_netting_agreement_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_netting_agreement_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_netting_agreement_version_request::key},
    };
}

/**
 * @brief The members of get_netting_agreement_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_netting_agreement_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_netting_agreement_version_response::result},
        utility::codec::field{"version", &get_netting_agreement_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::netting_agreement_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::netting_agreement_key& v);
    static ores::refdata::messaging::netting_agreement_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::netting_agreement_key& v);
    static ores::refdata::messaging::netting_agreement_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::netting_agreement_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::netting_agreement_write& v);
    static ores::refdata::messaging::netting_agreement_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::netting_agreement_write& v);
    static ores::refdata::messaging::netting_agreement_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::netting_agreement_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::netting_agreement_change& v);
    static ores::refdata::messaging::netting_agreement_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::netting_agreement_change& v);
    static ores::refdata::messaging::netting_agreement_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::netting_agreement_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::netting_agreement_removal& v);
    static ores::refdata::messaging::netting_agreement_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::netting_agreement_removal& v);
    static ores::refdata::messaging::netting_agreement_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::netting_agreement_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::netting_agreement_lookup& v);
    static ores::refdata::messaging::netting_agreement_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::netting_agreement_lookup& v);
    static ores::refdata::messaging::netting_agreement_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::netting_agreements_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::netting_agreements_filter& v);
    static ores::refdata::messaging::netting_agreements_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::netting_agreements_filter& v);
    static ores::refdata::messaging::netting_agreements_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::netting_agreement_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::netting_agreement_event& v);
    static ores::refdata::messaging::netting_agreement_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::netting_agreement_event& v);
    static ores::refdata::messaging::netting_agreement_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::netting_agreement_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::netting_agreement_version_key& v);
    static ores::refdata::messaging::netting_agreement_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::netting_agreement_version_key& v);
    static ores::refdata::messaging::netting_agreement_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::netting_agreement_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::netting_agreement_versions_filter& v);
    static ores::refdata::messaging::netting_agreement_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::netting_agreement_versions_filter& v);
    static ores::refdata::messaging::netting_agreement_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_netting_agreements_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_netting_agreements_request& v);
    static ores::refdata::messaging::list_netting_agreements_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_netting_agreements_request& v);
    static ores::refdata::messaging::list_netting_agreements_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_netting_agreements_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_netting_agreements_response& v);
    static ores::refdata::messaging::list_netting_agreements_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_netting_agreements_response& v);
    static ores::refdata::messaging::list_netting_agreements_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_netting_agreement_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_netting_agreement_request& v);
    static ores::refdata::messaging::get_netting_agreement_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_netting_agreement_request& v);
    static ores::refdata::messaging::get_netting_agreement_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_netting_agreement_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_netting_agreement_response& v);
    static ores::refdata::messaging::get_netting_agreement_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_netting_agreement_response& v);
    static ores::refdata::messaging::get_netting_agreement_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_netting_agreements_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_netting_agreements_request& v);
    static ores::refdata::messaging::get_many_netting_agreements_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_netting_agreements_request& v);
    static ores::refdata::messaging::get_many_netting_agreements_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_netting_agreements_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_netting_agreements_response& v);
    static ores::refdata::messaging::get_many_netting_agreements_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_netting_agreements_response& v);
    static ores::refdata::messaging::get_many_netting_agreements_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_netting_agreement_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_netting_agreement_request& v);
    static ores::refdata::messaging::put_netting_agreement_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_netting_agreement_request& v);
    static ores::refdata::messaging::put_netting_agreement_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_netting_agreement_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_netting_agreement_response& v);
    static ores::refdata::messaging::put_netting_agreement_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_netting_agreement_response& v);
    static ores::refdata::messaging::put_netting_agreement_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_netting_agreements_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_netting_agreements_request& v);
    static ores::refdata::messaging::put_many_netting_agreements_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_netting_agreements_request& v);
    static ores::refdata::messaging::put_many_netting_agreements_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_netting_agreements_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_netting_agreements_response& v);
    static ores::refdata::messaging::put_many_netting_agreements_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_netting_agreements_response& v);
    static ores::refdata::messaging::put_many_netting_agreements_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_netting_agreement_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_netting_agreement_request& v);
    static ores::refdata::messaging::delete_netting_agreement_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_netting_agreement_request& v);
    static ores::refdata::messaging::delete_netting_agreement_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_netting_agreement_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_netting_agreement_response& v);
    static ores::refdata::messaging::delete_netting_agreement_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_netting_agreement_response& v);
    static ores::refdata::messaging::delete_netting_agreement_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_netting_agreements_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_netting_agreements_request& v);
    static ores::refdata::messaging::delete_many_netting_agreements_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_netting_agreements_request& v);
    static ores::refdata::messaging::delete_many_netting_agreements_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_netting_agreements_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_netting_agreements_response& v);
    static ores::refdata::messaging::delete_many_netting_agreements_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_netting_agreements_response& v);
    static ores::refdata::messaging::delete_many_netting_agreements_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_counterparty_id_netting_agreements_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_by_counterparty_id_netting_agreements_request& v);
    static ores::refdata::messaging::list_by_counterparty_id_netting_agreements_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_counterparty_id_netting_agreements_request& v);
    static ores::refdata::messaging::list_by_counterparty_id_netting_agreements_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_counterparty_id_netting_agreements_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_by_counterparty_id_netting_agreements_response& v);
    static ores::refdata::messaging::list_by_counterparty_id_netting_agreements_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_counterparty_id_netting_agreements_response& v);
    static ores::refdata::messaging::list_by_counterparty_id_netting_agreements_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_netting_agreement_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_netting_agreement_versions_request& v);
    static ores::refdata::messaging::list_netting_agreement_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_netting_agreement_versions_request& v);
    static ores::refdata::messaging::list_netting_agreement_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_netting_agreement_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_netting_agreement_versions_response& v);
    static ores::refdata::messaging::list_netting_agreement_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_netting_agreement_versions_response& v);
    static ores::refdata::messaging::list_netting_agreement_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_netting_agreement_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_netting_agreement_version_request& v);
    static ores::refdata::messaging::get_netting_agreement_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_netting_agreement_version_request& v);
    static ores::refdata::messaging::get_netting_agreement_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_netting_agreement_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_netting_agreement_version_response& v);
    static ores::refdata::messaging::get_netting_agreement_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_netting_agreement_version_response& v);
    static ores::refdata::messaging::get_netting_agreement_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
