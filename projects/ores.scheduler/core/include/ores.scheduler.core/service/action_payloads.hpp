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
#ifndef ORES_SCHEDULER_CORE_SERVICE_ACTION_PAYLOADS_HPP
#define ORES_SCHEDULER_CORE_SERVICE_ACTION_PAYLOADS_HPP

#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/shaped_wire_traits.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <tuple>

namespace ores::scheduler::service {

/**
 * @brief Parsed fields from the action_payload JSON.
 */
struct mq_payload_fields {
    std::string queue_id;
    std::string message_type;
    std::string payload = "{}";
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<mq_payload_fields>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"queue_id", &mq_payload_fields::queue_id},
        utility::codec::field{"message_type", &mq_payload_fields::message_type},
        utility::codec::field{"payload", &mq_payload_fields::payload}};
}
static_assert(utility::codec::names_every_member<mq_payload_fields>());

// Fields read from job_definition.action_payload.
struct nats_publish_payload {
    std::string subject;
    std::optional<std::string> report_definition_id;
    std::optional<std::string> tenant_id;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<nats_publish_payload>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"subject", &nats_publish_payload::subject},
        utility::codec::field{"report_definition_id", &nats_publish_payload::report_definition_id},
        utility::codec::field{"tenant_id", &nats_publish_payload::tenant_id}};
}
static_assert(utility::codec::names_every_member<nats_publish_payload>());

// Fields read from job_definition.action_payload for a job that sends an
// authenticated request and reads its answer. The body is the callee's own
// request; a job whose request needs arguments states them here.
struct nats_request_payload {
    std::string subject;
    std::optional<std::string> body;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<nats_request_payload>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"subject", &nats_request_payload::subject},
        utility::codec::field{"body", &nats_request_payload::body}};
}
static_assert(utility::codec::names_every_member<nats_request_payload>());

// The body published to the subject on each firing. It must name every member
// that reporting's trigger_report_instance_request requires: the receiver reads
// members by name, and one it requires and does not find fails the whole decode,
// so the run the firing asked for is never admitted. Nothing here states that
// contract: the guard is a reporting test that decodes the bytes this handler
// publishes as the production request type, so a member added to the contract
// breaks it.
struct nats_trigger_body {
    boost::uuids::uuid report_definition_id;
    boost::uuids::uuid tenant_id;
    std::int64_t job_instance_id = 0;
    std::string kind;
    // The run's cut inputs. A schedule states none of them: the covered date,
    // the scope and the rates centre belong to whoever asks for the run, so a
    // scheduled run records no resolved cut until the run itself states them.
    std::string covered_date;
    std::string scope_code;
    std::string business_centre_code;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<nats_trigger_body>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"report_definition_id", &nats_trigger_body::report_definition_id},
        utility::codec::field{"tenant_id", &nats_trigger_body::tenant_id},
        utility::codec::field{"job_instance_id", &nats_trigger_body::job_instance_id},
        utility::codec::field{"kind", &nats_trigger_body::kind},
        utility::codec::field{"covered_date", &nats_trigger_body::covered_date},
        utility::codec::field{"scope_code", &nats_trigger_body::scope_code},
        utility::codec::field{"business_centre_code", &nats_trigger_body::business_centre_code}};
}
static_assert(utility::codec::names_every_member<nats_trigger_body>());

/**
 * @brief The common result of the operation that answers a trigger.
 */
struct reply_result {
    ores::utility::domain::outcome outcome = ores::utility::domain::outcome::ok;
    std::string code;
    std::string message;
};

/**
 * @brief The part of an answer the handler reads: its result.
 */
struct reply_body {
    reply_result result;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<reply_result>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"outcome", &reply_result::outcome},
        utility::codec::field{"code", &reply_result::code},
        utility::codec::field{"message", &reply_result::message}};
}
static_assert(utility::codec::names_every_member<reply_result>());

[[nodiscard]] constexpr auto wire_fields(std::type_identity<reply_body>) {
    return ::ores::utility::codec::field_list{utility::codec::field{"result", &reply_body::result}};
}
static_assert(utility::codec::names_every_member<reply_body>());

}

template <>
struct ores::utility::codec::wire_traits<ores::scheduler::service::nats_trigger_body>
    : shaped_wire_traits<ores::scheduler::service::nats_trigger_body> {};

template <>
struct ores::utility::codec::wire_traits<ores::scheduler::service::reply_body>
    : shaped_wire_traits<ores::scheduler::service::reply_body> {};

#endif
