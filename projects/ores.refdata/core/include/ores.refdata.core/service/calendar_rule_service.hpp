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
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_CORE_SERVICE_CALENDAR_RULE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CALENDAR_RULE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/calendar_rule.hpp"
#include "ores.refdata.api/messaging/calendar_rule_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/calendar_rule_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing calendar rules.
 *
 * Provides a higher-level interface for calendar rule operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT calendar_rule_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.calendar_rule_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a calendar_rule_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit calendar_rule_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_calendar_rules_response
    list_calendar_rules(const messaging::list_calendar_rules_request& request);
    messaging::get_calendar_rule_response
    get_calendar_rule(const messaging::get_calendar_rule_request& request);
    messaging::get_many_calendar_rules_response
    get_many_calendar_rules(const messaging::get_many_calendar_rules_request& request);
    messaging::put_calendar_rule_response
    put_calendar_rule(const messaging::put_calendar_rule_request& request);
    messaging::put_many_calendar_rules_response
    put_many_calendar_rules(const messaging::put_many_calendar_rules_request& request);
    messaging::delete_calendar_rule_response
    delete_calendar_rule(const messaging::delete_calendar_rule_request& request);
    messaging::delete_many_calendar_rules_response
    delete_many_calendar_rules(const messaging::delete_many_calendar_rules_request& request);
    messaging::list_by_calendar_code_calendar_rules_response list_by_calendar_code_calendar_rules(
        const messaging::list_by_calendar_code_calendar_rules_request& request);
    messaging::list_calendar_rule_versions_response
    list_calendar_rule_versions(const messaging::list_calendar_rule_versions_request& request);
    messaging::get_calendar_rule_version_response
    get_calendar_rule_version(const messaging::get_calendar_rule_version_request& request);
    /**@}*/

    /**
     * @brief Lists calendar rules with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of calendar rules for the requested page.
     */
    std::vector<domain::calendar_rule> list_calendar_rules(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendar rules.
     *
     * @return Total number of active calendar rules.
     */
    std::uint32_t count_calendar_rules();


    /**
     * @brief Lists calendar rules filtered by calendar_code, with pagination.
     *
     * @param calendar_code The calendar_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching calendar rules for the requested page.
     */
    std::vector<domain::calendar_rule> list_calendar_rules_by_calendar_code(
        const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendar rules filtered by calendar_code.
     *
     * @param calendar_code The calendar_code to filter by.
     * @return Total number of matching calendar rules.
     */
    std::uint32_t count_calendar_rules_by_calendar_code(const std::string& calendar_code);


    /**
     * @brief Lists calendar rules filtered by calendar_code that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param calendar_code The calendar_code to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching calendar rules.
     */
    std::vector<domain::calendar_rule> list_calendar_rules_by_calendar_code_as_of(
        const std::string& calendar_code,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);

    /**
     * @brief Retrieves a single calendar rule as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The calendar rule at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::calendar_rule> get_calendar_rule_at_version(const std::string& id,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single calendar rule by its primary key.
     *
     * @return The calendar rule if found, std::nullopt otherwise.
     */
    std::optional<domain::calendar_rule> get_calendar_rule(const std::string& id);

    /**
     * @brief Retrieves a batch of calendar rules by primary key.
     */
    std::vector<domain::calendar_rule> get_calendar_rules(const std::vector<std::string>& ids);

    /**
     * @brief Saves a calendar rule (creates or updates).
     *
     * @param calendar_rule The calendar rule to save.
     * @throws std::exception on failure.
     */
    void save_calendar_rule(const domain::calendar_rule& calendar_rule);

    /**
     * @brief Saves a batch of calendar rules.
     *
     * @param calendar_rules The calendar rules to save.
     * @throws std::exception on failure.
     */
    void save_calendar_rules(const std::vector<domain::calendar_rule>& calendar_rules);

    /**
     * @brief Deletes a calendar rule by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_calendar_rule(const std::string& id);

    /**
     * @brief Deletes calendar rules by their primary keys.
     */
    void delete_calendar_rules(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a calendar rule.
     */
    std::vector<domain::calendar_rule> get_calendar_rule_history(const std::string& id);

private:
    context ctx_;
    repository::calendar_rule_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result prepare_change(const messaging::calendar_rule_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::calendar_rule& out);
};

}

#endif
