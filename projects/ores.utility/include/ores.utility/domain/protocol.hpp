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
#ifndef ORES_UTILITY_DOMAIN_PROTOCOL_HPP
#define ORES_UTILITY_DOMAIN_PROTOCOL_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::utility::domain {

/**
 * @brief How a request ended.
 *
 * Closed on purpose: a caller branches on these seven and on nothing else.
 * Everything an operation decides is reported here, in the response body.
 * A failure that stops an operation being reached at all -- an unauthenticated
 * connection, an unparseable request -- is reported by the envelope instead,
 * because there is no body to carry it.
 */
enum class outcome {
    ok,
    invalid,
    denied,
    missing,
    conflict,
    unavailable,
    failed
};

/**
 * @brief One field a request got wrong, named so a caller can act on it.
 */
struct field_failure {
    std::string field;
    std::string code;
    std::string message;
};

/**
 * @brief The result every response carries.
 *
 * A payload's data is meaningful only when @c outcome is @c ok ; on any other
 * outcome there is no data, and a caller that reads data without checking the
 * outcome is wrong.
 */
struct result {
    ores::utility::domain::outcome outcome = ores::utility::domain::outcome::ok;
    std::string code;
    std::string message;
    std::vector<field_failure> fields;
};

/**
 * @brief What a write believes about the row it is about to change.
 *
 * Three distinct claims, so that no integer has to double as a mode:
 * @c any replaces whatever is there, @c must_not_exist makes a duplicate
 * create detectable, and @c must_match_version makes a concurrent edit
 * detectable. A write that states none is invalid.
 */
enum class precondition_kind {
    any,
    must_not_exist,
    must_match_version
};

struct precondition {
    precondition_kind kind = precondition_kind::must_not_exist;
    std::optional<std::uint32_t> version;
};

/**
 * @brief The precondition a removal states when it states none.
 *
 * A removal that required the row to be absent would never be satisfiable, so
 * the default is the claim a removal actually makes: remove whatever is
 * current. A caller that wants the version checked says so, and states the
 * version.
 */
inline constexpr precondition removal_precondition{
    precondition_kind::any, std::nullopt};

/**
 * @brief Why a write is being made. User-owned, unlike the audit provenance,
 * which the service derives from the authenticated context.
 */
struct change_intent {
    std::string reason_code;
    std::string commentary;
};

/**
 * @brief The order a page is returned in.
 *
 * Stated rather than implied, because a page of an unordered set is not
 * reproducible and paging through it can repeat or skip rows. An empty @c
 * field means the order by key, which is what makes a caller that names no
 * order still get a stable page.
 */
struct order {
    std::string field;
    bool descending = false;
};

/**
 * @brief How much of a tree a scoped read covers.
 *
 * One verb with two answers rather than two operations: reading a node's
 * children and reading everything beneath it differ only in reach, so a
 * caller states which it means and the read is otherwise identical. An
 * implementation that cannot answer @c subtree says so, rather than
 * silently answering with the children.
 */
enum class scope {
    direct,
    subtree
};

}

#endif
