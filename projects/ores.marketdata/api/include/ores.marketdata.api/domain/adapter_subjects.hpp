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
#include <algorithm>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

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

/**
 * @brief The two subject families the ingest loop consumes, as the relative wildcards a
 * subscription takes.
 *
 * The loop subscribes to exactly these two values and to nothing else, so a publisher's subject
 * being in one of these families and the loop subscribing to it are one fact rather than two
 * that can drift. A test that pins a feed's publish subject to the ingest reads this value
 * rather than spelling a wildcard of its own.
 */
struct ingest_subjects final {
    /// The synthetic producers that name no adapter, under "synthetic.v1.ops.tick.>".
    std::string producers;
    /// Every adapter's ingress, under "marketdata.v1.ingress.>".
    std::string adapters;

    /// Both families, in the order a caller may walk them.
    [[nodiscard]] std::vector<std::string> all() const {
        return {producers, adapters};
    }
};

/// The subject families the ingest loop consumes. It subscribes to each of them once.
inline ingest_subjects ingest_subject_wildcards() {
    return {.producers = std::string(synthetic_tick_subject).append(".>"),
            .adapters = adapter_ingress_subject_all()};
}

/// The subject split into its tokens, as NATS counts them.
inline std::vector<std::string_view> subject_tokens_of(std::string_view subject) {
    std::vector<std::string_view> tokens;
    std::size_t at = 0;
    while (at <= subject.size()) {
        const auto dot = subject.find('.', at);
        const auto end = dot == std::string_view::npos ? subject.size() : dot;
        tokens.push_back(subject.substr(at, end - at));
        if (dot == std::string_view::npos)
            break;
        at = dot + 1;
    }
    return tokens;
}

/**
 * @brief Whether @p wildcard, as a subscription takes it, covers @p subject.
 *
 * A @c ">" token matches one token or more, and a @c "*" token matches exactly one. Both
 * subjects are the relative form: the environment prefix that a client adds is outside this
 * comparison.
 */
inline bool subject_matches(std::string_view wildcard, std::string_view subject) {
    const auto wanted = subject_tokens_of(wildcard);
    const auto actual = subject_tokens_of(subject);
    // A subject or a wildcard with an empty token is one NATS refuses, so it is none of the
    // loop's, whatever its shape.
    const auto empty = [](auto token) {
        return token.empty();
    };
    if (std::ranges::any_of(actual, empty) || std::ranges::any_of(wanted, empty))
        return false;
    for (std::size_t i = 0; i < wanted.size(); ++i) {
        if (wanted[i] == ">")
            return actual.size() > i;
        if (i >= actual.size())
            return false;
        if (wanted[i] != "*" && wanted[i] != actual[i])
            return false;
    }
    return actual.size() == wanted.size();
}

/// True when one of the ingest loop's subscriptions covers @p subject.
inline bool ingest_consumes(std::string_view subject) {
    return std::ranges::any_of(ingest_subject_wildcards().all(),
                               [subject](const auto& w) { return subject_matches(w, subject); });
}

}

#endif
