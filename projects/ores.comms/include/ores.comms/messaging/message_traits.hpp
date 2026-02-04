/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_COMMS_MESSAGING_MESSAGE_TRAITS_HPP
#define ORES_COMMS_MESSAGING_MESSAGE_TRAITS_HPP

#include <concepts>
#include "ores.comms/messaging/message_type.hpp"

namespace ores::comms::messaging {

/**
 * @brief Traits template for mapping request types to their response types
 * and message type enum values.
 *
 * Each request/response message pair should specialize this template to
 * provide compile-time type information. This enables simplified API usage
 * where only the request type needs to be specified.
 *
 * Example specialization:
 * @code
 *     template<>
 *     struct message_traits<subscribe_request> {
 *         using request_type = subscribe_request;
 *         using response_type = subscribe_response;
 *         static constexpr message_type request_message_type =
 *             message_type::subscribe_request;
 *     };
 * @endcode
 *
 * Usage with process_request:
 * @code
 *     // Old API (verbose):
 *     auto result = client_->process_request<
 *         subscribe_request,
 *         subscribe_response,
 *         message_type::subscribe_request>(std::move(req));
 *
 *     // New API (using traits):
 *     auto result = client_->process_request(std::move(req));
 * @endcode
 *
 * @note This is complementary to event_traits in ores.eventing, which maps
 * domain events to string names for the pub/sub notification system.
 * message_traits maps request types to response types for RPC-style messaging.
 *
 * @tparam Request The request type.
 */
template<typename Request>
struct message_traits {
    // Primary template is intentionally not defined.
    // Each request type must provide its own specialization.
    // This causes a compile-time error if a request is used without traits.
};

/**
 * @brief Concept for types that have message_traits specialization.
 *
 * A type satisfies has_message_traits if its message_traits specialization
 * provides:
 * - request_type: The request struct type
 * - response_type: The corresponding response struct type
 * - request_message_type: The message_type enum value for the request
 */
template<typename T>
concept has_message_traits = requires {
    typename message_traits<T>::request_type;
    typename message_traits<T>::response_type;
    { message_traits<T>::request_message_type } -> std::convertible_to<message_type>;
};

}

#endif
