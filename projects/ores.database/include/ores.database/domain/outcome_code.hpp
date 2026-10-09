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
 * Template: cpp_outcome_code.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_DATABASE_DOMAIN_OUTCOME_CODE_HPP
#define ORES_DATABASE_DOMAIN_OUTCOME_CODE_HPP

#include "ores.utility/domain/protocol.hpp"
#include <string>
#include <string_view>

namespace ores::database::domain {

/**
 * @brief The values a refusal's message is filled from.
 *
 * Every name in the outcome catalogue's argument vocabulary is a member here,
 * whether or not the outcome being described uses it. A message names only the
 * members its own entry declared, so an unfilled member is never read.
 */
struct outcome_args {
    std::string entity;
    std::string field;
    std::string value;
    std::string expected;
    std::string current;
    std::string reason;
    std::string limit;
};

/**
 * @brief Every way this system refuses a request.
 *
 * Rendered from the outcome catalogue, which is the single source of truth for
 * the code, the coarse outcome, the SQLSTATE and the wording. A store outcome
 * is refused by a PostgreSQL trigger, which composes the same sentence through
 * the matching ores_outcome_<code>_fn; a request outcome is refused here.
 */
enum class outcome_code {
    already_exists,
    version_conflict,
    missing_field,
    level_violation,
    not_found,
    order_not_supported,
    filter_not_supported,
    filter_too_large,
    as_of_invalid,
    scope_not_supported,
    relation_required,
    precondition_not_supported,
    precondition_incomplete,
    batch_removal_is_unconditional,
    internal_error,
};

/**
 * @brief The string a reply carries in @c result.code.
 */
[[nodiscard]] constexpr std::string_view to_string(outcome_code v) noexcept {
    switch (v) {
        case outcome_code::already_exists:
            return "already_exists";
        case outcome_code::version_conflict:
            return "version_conflict";
        case outcome_code::missing_field:
            return "missing_field";
        case outcome_code::level_violation:
            return "level_violation";
        case outcome_code::not_found:
            return "not_found";
        case outcome_code::order_not_supported:
            return "order_not_supported";
        case outcome_code::filter_not_supported:
            return "filter_not_supported";
        case outcome_code::filter_too_large:
            return "filter_too_large";
        case outcome_code::as_of_invalid:
            return "as_of_invalid";
        case outcome_code::scope_not_supported:
            return "scope_not_supported";
        case outcome_code::relation_required:
            return "relation_required";
        case outcome_code::precondition_not_supported:
            return "precondition_not_supported";
        case outcome_code::precondition_incomplete:
            return "precondition_incomplete";
        case outcome_code::batch_removal_is_unconditional:
            return "batch_removal_is_unconditional";
        case outcome_code::internal_error:
            return "internal_error";
        default:
            return {};
    }
}

/**
 * @brief The coarse outcome a reply carries for @p v.
 */
[[nodiscard]] constexpr ores::utility::domain::outcome outcome_of(outcome_code v) noexcept {
    switch (v) {
        case outcome_code::already_exists:
            return ores::utility::domain::outcome::conflict;
        case outcome_code::version_conflict:
            return ores::utility::domain::outcome::conflict;
        case outcome_code::missing_field:
            return ores::utility::domain::outcome::invalid;
        case outcome_code::level_violation:
            return ores::utility::domain::outcome::invalid;
        case outcome_code::not_found:
            return ores::utility::domain::outcome::missing;
        case outcome_code::order_not_supported:
            return ores::utility::domain::outcome::invalid;
        case outcome_code::filter_not_supported:
            return ores::utility::domain::outcome::invalid;
        case outcome_code::filter_too_large:
            return ores::utility::domain::outcome::invalid;
        case outcome_code::as_of_invalid:
            return ores::utility::domain::outcome::invalid;
        case outcome_code::scope_not_supported:
            return ores::utility::domain::outcome::invalid;
        case outcome_code::relation_required:
            return ores::utility::domain::outcome::invalid;
        case outcome_code::precondition_not_supported:
            return ores::utility::domain::outcome::invalid;
        case outcome_code::precondition_incomplete:
            return ores::utility::domain::outcome::invalid;
        case outcome_code::batch_removal_is_unconditional:
            return ores::utility::domain::outcome::invalid;
        case outcome_code::internal_error:
            return ores::utility::domain::outcome::failed;
        default:
            return ores::utility::domain::outcome::failed;
    }
}

namespace detail {

inline void replace_all(std::string& text, std::string_view from, std::string_view to) {
    for (auto pos = text.find(from); pos != std::string::npos;
         pos = text.find(from, pos + to.size()))
        text.replace(pos, from.size(), to);
}

/**
 * @brief Fills a message template's {name} placeholders from @p a.
 *
 * The one renderer, shared by every outcome. It substitutes each name whether
 * the template names it or not, so a template that names a subset is filled
 * from the same call as one that names every member.
 */
inline std::string fill(std::string_view tmpl, const outcome_args& a) {
    std::string text(tmpl);
    replace_all(text, "{entity}", a.entity);
    replace_all(text, "{field}", a.field);
    replace_all(text, "{value}", a.value);
    replace_all(text, "{expected}", a.expected);
    replace_all(text, "{current}", a.current);
    replace_all(text, "{reason}", a.reason);
    replace_all(text, "{limit}", a.limit);
    return text;
}

}

/**
 * @brief The sentence a caller reads for @p v, filled from @p a.
 *
 * The same sentence the store composes through ores_outcome_<code>_fn, so a
 * client cannot tell which side refused from the words alone.
 */
[[nodiscard]] inline std::string describe(outcome_code v, const outcome_args& a) {
    switch (v) {
        case outcome_code::already_exists:
            return detail::fill("The {entity} already exists for that {field}. State the version "
                                "you read to replace it, or ask for a version replace.",
                                a);
        case outcome_code::version_conflict:
            return detail::fill("The {entity} for {field} is at version {current}, and this write "
                                "states version {expected}.",
                                a);
        case outcome_code::missing_field:
            return detail::fill("Invalid {entity}: value cannot be null or empty.", a);
        case outcome_code::level_violation:
            return detail::fill(
                "The {entity} states {field} at level {expected}, and its parent unit is at level "
                "{current}. A child level must be greater than its parent's.",
                a);
        case outcome_code::not_found:
            return detail::fill("The {entity} does not exist.", a);
        case outcome_code::order_not_supported:
            return detail::fill("A list of {entity} cannot be ordered by {field}.", a);
        case outcome_code::filter_not_supported:
            return detail::fill("Filtering is not served for this {entity} yet.", a);
        case outcome_code::filter_too_large:
            return detail::fill("The filter on {field} lists more than {limit} values.", a);
        case outcome_code::as_of_invalid:
            return detail::fill("The as-of time {value} is not a valid instant.", a);
        case outcome_code::scope_not_supported:
            return detail::fill(
                "This read of the {entity} has no subtree: it reads its direct members.", a);
        case outcome_code::relation_required:
            return detail::fill(
                "This read of the {entity} is scoped by {field}, and the request states none.", a);
        case outcome_code::precondition_not_supported:
            return detail::fill("A removal cannot require that a row is absent.", a);
        case outcome_code::precondition_incomplete:
            return detail::fill("A versioned removal must state the version it expects.", a);
        case outcome_code::batch_removal_is_unconditional:
            return detail::fill("A batch removal is unconditional; remove the rows one at a time "
                                "to state a version.",
                                a);
        case outcome_code::internal_error:
            return detail::fill("The store refused the write: {reason}", a);
        default:
            return {};
    }
}

/**
 * @brief The reply for a refusal the caller can name but not describe.
 *
 * A repository reports a status, not the row behind it, so a service that
 * refuses on that status has no arguments to fill a sentence with. The code
 * and the coarse outcome still come from the catalogue, so nothing restates
 * them; the reply carries no message.
 */
[[nodiscard]] inline ores::utility::domain::result refuse(outcome_code v) {
    ores::utility::domain::result r;
    r.outcome = outcome_of(v);
    r.code = std::string(to_string(v));
    return r;
}

/**
 * @brief The reply for a refusal, with its catalogue sentence filled from @p a.
 *
 * The one constructor for a described refusal, so a service states which
 * outcome it reached and nothing else about how to say it.
 */
[[nodiscard]] inline ores::utility::domain::result refuse(outcome_code v, const outcome_args& a) {
    auto r = refuse(v);
    r.message = describe(v, a);
    return r;
}

}

#endif
