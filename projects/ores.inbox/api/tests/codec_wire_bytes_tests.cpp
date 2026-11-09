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
#include "ores.inbox.api/domain/approval_decision_codec.hpp"
#include "ores.inbox.api/domain/approval_decision_type_codec.hpp"
#include "ores.inbox.api/domain/approval_kind_codec.hpp"
#include "ores.inbox.api/domain/approval_kind_part_codec.hpp"
#include "ores.inbox.api/domain/approval_part_codec.hpp"
#include "ores.inbox.api/domain/approval_policy_codec.hpp"
#include "ores.inbox.api/domain/approval_request_codec.hpp"
#include "ores.inbox.api/domain/approval_request_part_codec.hpp"
#include "ores.inbox.api/domain/approval_request_state_codec.hpp"
#include "ores.inbox.api/domain/delivery_outcome_type_codec.hpp"
#include "ores.inbox.api/domain/hold_reason_codec.hpp"
#include "ores.inbox.api/domain/notification_argument_codec.hpp"
#include "ores.inbox.api/domain/notification_channel_codec.hpp"
#include "ores.inbox.api/domain/notification_codec.hpp"
#include "ores.inbox.api/domain/notification_delivery_codec.hpp"
#include "ores.inbox.api/domain/notification_kind_codec.hpp"
#include "ores.inbox.api/domain/notification_preference_codec.hpp"
#include "ores.inbox.api/domain/notification_recipient_codec.hpp"
#include "ores.inbox.api/generators/approval_decision_generator.hpp"
#include "ores.inbox.api/generators/approval_decision_type_generator.hpp"
#include "ores.inbox.api/generators/approval_kind_generator.hpp"
#include "ores.inbox.api/generators/approval_kind_part_generator.hpp"
#include "ores.inbox.api/generators/approval_part_generator.hpp"
#include "ores.inbox.api/generators/approval_policy_generator.hpp"
#include "ores.inbox.api/generators/approval_request_generator.hpp"
#include "ores.inbox.api/generators/approval_request_part_generator.hpp"
#include "ores.inbox.api/generators/approval_request_state_generator.hpp"
#include "ores.inbox.api/generators/delivery_outcome_type_generator.hpp"
#include "ores.inbox.api/generators/hold_reason_generator.hpp"
#include "ores.inbox.api/generators/notification_argument_generator.hpp"
#include "ores.inbox.api/generators/notification_channel_generator.hpp"
#include "ores.inbox.api/generators/notification_delivery_generator.hpp"
#include "ores.inbox.api/generators/notification_generator.hpp"
#include "ores.inbox.api/generators/notification_kind_generator.hpp"
#include "ores.inbox.api/generators/notification_preference_generator.hpp"
#include "ores.inbox.api/generators/notification_recipient_generator.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.inbox.api/messaging/notification_operations_protocol.hpp"
#include "ores.inbox.api/workflow/apply_step_command.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <catch2/catch_test_macros.hpp>
#include <span>
#include <string>

namespace {

const std::string codec_tags("[codec]");

std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief The codec reads back what it wrote, in both formats.
 */
template <typename T>
void survives_the_wire(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;
    CHECK(codec::is_generated);

    const auto json = codec::write_json(v);
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(codec::write_msgpack(codec::read_msgpack(bytes_of(msgpack))) == msgpack);
}

template <typename... T>
void every_default_has_the_same_bytes() {
    (survives_the_wire(T{}), ...);
}

}

TEST_CASE("every inbox domain type has its wire form", codec_tags) {
    ores::utility::generation::generation_context ctx(20261102);
    survives_the_wire(ores::inbox::generators::generate_synthetic_approval_decision(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_approval_decision_type(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_approval_kind(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_approval_kind_part(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_approval_part(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_approval_policy(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_approval_request(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_approval_request_part(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_approval_request_state(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_delivery_outcome_type(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_hold_reason(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_notification(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_notification_argument(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_notification_channel(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_notification_delivery(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_notification_kind(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_notification_preference(ctx));
    survives_the_wire(ores::inbox::generators::generate_synthetic_notification_recipient(ctx));
}

TEST_CASE("every inbox operation message's default has its wire form", codec_tags) {
    every_default_has_the_same_bytes<ores::inbox::messaging::raise_approval_request_request,
                                     ores::inbox::messaging::raise_approval_request_response,
                                     ores::inbox::messaging::withdraw_approval_request_request,
                                     ores::inbox::messaging::withdraw_approval_request_response,
                                     ores::inbox::messaging::decide_approval_request_request,
                                     ores::inbox::messaging::decide_approval_request_response,
                                     ores::inbox::messaging::list_approval_queue_request,
                                     ores::inbox::messaging::list_approval_queue_response,
                                     ores::inbox::messaging::list_my_approval_requests_request,
                                     ores::inbox::messaging::list_my_approval_requests_response,
                                     ores::inbox::messaging::get_approval_request,
                                     ores::inbox::messaging::get_approval_response,
                                     ores::inbox::messaging::expire_overdue_approvals_request,
                                     ores::inbox::messaging::expire_overdue_approvals_response,
                                     ores::inbox::messaging::remind_expiring_approvals_request,
                                     ores::inbox::messaging::remind_expiring_approvals_response,
                                     ores::inbox::messaging::retry_stalled_applies_request,
                                     ores::inbox::messaging::retry_stalled_applies_response,
                                     ores::inbox::messaging::approval_request_field,
                                     ores::inbox::messaging::approval_request_version,
                                     ores::inbox::messaging::get_approval_history_request,
                                     ores::inbox::messaging::get_approval_history_response,
                                     ores::inbox::messaging::approval_part_progress,
                                     ores::inbox::messaging::get_approval_parts_request,
                                     ores::inbox::messaging::get_approval_parts_response,
                                     ores::inbox::messaging::notification_argument_value,
                                     ores::inbox::messaging::inbox_notification,
                                     ores::inbox::messaging::raise_notification_request,
                                     ores::inbox::messaging::raise_notification_response,
                                     ores::inbox::messaging::list_my_notifications_request,
                                     ores::inbox::messaging::list_my_notifications_response,
                                     ores::inbox::messaging::count_unread_notifications_request,
                                     ores::inbox::messaging::count_unread_notifications_response,
                                     ores::inbox::messaging::mark_notifications_read_request,
                                     ores::inbox::messaging::mark_notifications_read_response,
                                     ores::inbox::messaging::clear_notifications_request,
                                     ores::inbox::messaging::clear_notifications_response>();
}

TEST_CASE("the apply step command has its wire form", codec_tags) {
    const ores::inbox::workflow::apply_step_command command{"r1"};
}
