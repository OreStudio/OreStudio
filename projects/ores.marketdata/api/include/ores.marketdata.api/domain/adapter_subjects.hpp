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
#ifndef ORES_MARKETDATA_API_DOMAIN_ADAPTER_SUBJECTS_HPP
#define ORES_MARKETDATA_API_DOMAIN_ADAPTER_SUBJECTS_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The subjects an adapter and the market data service meet on.
 *
 * Control messages use "marketdata.v1.adapters.*". A tick travels on
 * "marketdata.v1.ingress.<adapter>.<id token>". The adapter id and the vendor
 * id are opaque text, so each becomes one subject token through
 * @c subject_token(), and nothing is decoded to learn what an adapter is. No
 * subject holds a mark of the source kind.
 */
inline constexpr std::string_view adapter_subject_base = "marketdata.v1.adapters";
inline constexpr std::string_view adapter_ingress_subject_base = "marketdata.v1.ingress";

/// The subject an adapter announces itself on.
inline std::string adapter_announce_subject() {
    return std::string(adapter_subject_base).append(".announce");
}

/// The subject the service sends @p adapter_id its demand on.
inline std::string adapter_demand_subject(std::string_view adapter_id) {
    return std::string(adapter_subject_base)
        .append(".")
        .append(subject_token(adapter_id))
        .append(".demand");
}

/// The subject @p adapter_id sends its heartbeat on.
inline std::string adapter_heartbeat_subject(std::string_view adapter_id) {
    return std::string(adapter_subject_base)
        .append(".")
        .append(subject_token(adapter_id))
        .append(".heartbeat");
}

/// The subject that covers the heartbeat of every adapter. The service subscribes once to it.
inline std::string adapter_heartbeat_subject_all() {
    return std::string(adapter_subject_base).append(".*.heartbeat");
}

/**
 * @brief The adapter id in a heartbeat subject, or nothing for a subject that
 * @c adapter_heartbeat_subject() could not have built.
 */
inline std::optional<std::string> adapter_heartbeat_subject_adapter(std::string_view subject) {
    const std::string prefix = std::string(adapter_subject_base) + ".";
    constexpr std::string_view suffix = ".heartbeat";
    const auto at = subject.find(prefix);
    if (at == std::string_view::npos)
        return std::nullopt;
    const auto rest = subject.substr(at + prefix.size());
    if (!rest.ends_with(suffix) || rest.size() <= suffix.size())
        return std::nullopt;
    const auto token = rest.substr(0, rest.size() - suffix.size());
    auto adapter = subject_token_text(token);
    if (!adapter || adapter->empty())
        return std::nullopt;
    return adapter;
}

/**
 * @brief The subject a tick of @p external_id travels on when @p adapter_id publishes it.
 *
 * The subject names the adapter and the external id only. The id scheme and the field travel
 * in the tick, so a reader takes them from the payload and never from the subject.
 */
inline std::string adapter_ingress_subject(std::string_view adapter_id,
                                           std::string_view external_id) {
    return std::string(adapter_ingress_subject_base)
        .append(".")
        .append(subject_token(adapter_id))
        .append(".")
        .append(subject_token(external_id));
}

/// The subject that covers the ticks of every adapter. The service subscribes once to it.
inline std::string adapter_ingress_subject_all() {
    return std::string(adapter_ingress_subject_base).append(".>");
}

/// The subject that covers the ticks of one adapter.
inline std::string adapter_ingress_subject_for(std::string_view adapter_id) {
    return std::string(adapter_ingress_subject_base)
        .append(".")
        .append(subject_token(adapter_id))
        .append(".>");
}

/// The adapter and the external id an ingress subject was built from.
struct adapter_ingress_parts {
    std::string adapter_id;
    std::string external_id;
};

/**
 * @brief The adapter id and the external id in an ingress subject, or nothing
 * for a subject that @c adapter_ingress_subject() could not have built.
 */
inline std::optional<adapter_ingress_parts>
adapter_ingress_subject_parts(std::string_view subject) {
    const std::string prefix = std::string(adapter_ingress_subject_base) + ".";
    if (!subject.starts_with(prefix))
        return std::nullopt;
    const auto rest = subject.substr(prefix.size());
    const auto dot = rest.find('.');
    if (dot == std::string_view::npos || rest.find('.', dot + 1) != std::string_view::npos)
        return std::nullopt;
    auto adapter = subject_token_text(rest.substr(0, dot));
    auto external = subject_token_text(rest.substr(dot + 1));
    if (!adapter || !external || adapter->empty() || external->empty())
        return std::nullopt;
    return adapter_ingress_parts{std::move(*adapter), std::move(*external)};
}

}

#endif
