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
#ifndef ORES_MARKETDATA_CORE_SERVICE_SERIES_CLASSIFICATION_RULE_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_SERIES_CLASSIFICATION_RULE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/series_classification_rule.hpp"
#include "ores.marketdata.api/messaging/series_classification_rule_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/series_classification_rule_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing series classification rules.
 *
 * Provides a higher-level interface for series classification rule operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT series_classification_rule_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.series_classification_rule_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a series_classification_rule_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit series_classification_rule_service(context ctx);

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
    messaging::list_series_classification_rules_response list_series_classification_rules(
        const messaging::list_series_classification_rules_request& request);
    messaging::get_series_classification_rule_response get_series_classification_rule(
        const messaging::get_series_classification_rule_request& request);
    messaging::get_many_series_classification_rules_response get_many_series_classification_rules(
        const messaging::get_many_series_classification_rules_request& request);
    messaging::put_series_classification_rule_response put_series_classification_rule(
        const messaging::put_series_classification_rule_request& request);
    messaging::put_many_series_classification_rules_response put_many_series_classification_rules(
        const messaging::put_many_series_classification_rules_request& request);
    messaging::delete_series_classification_rule_response delete_series_classification_rule(
        const messaging::delete_series_classification_rule_request& request);
    messaging::delete_many_series_classification_rules_response
    delete_many_series_classification_rules(
        const messaging::delete_many_series_classification_rules_request& request);
    messaging::list_series_classification_rule_versions_response
    list_series_classification_rule_versions(
        const messaging::list_series_classification_rule_versions_request& request);
    messaging::get_series_classification_rule_version_response
    get_series_classification_rule_version(
        const messaging::get_series_classification_rule_version_request& request);
    /**@}*/

    /**
     * @brief Lists series classification rules with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of series classification rules for the requested page.
     */
    std::vector<domain::series_classification_rule> list_rules(std::uint32_t offset,
                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active series classification rules.
     *
     * @return Total number of active series classification rules.
     */
    std::uint32_t count_rules();


    /**
     * @brief Retrieves a single series classification rule as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The series classification rule at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::series_classification_rule> get_rule_at_version(
        const std::string& series_type, const std::string& metric, std::uint32_t version);

    /**
     * @brief Retrieves a single series classification rule by its primary key.
     *
     * @return The series classification rule if found, std::nullopt otherwise.
     */
    std::optional<domain::series_classification_rule> get_rule(const std::string& series_type,
                                                               const std::string& metric);

    /**
     * @brief Retrieves a batch of series classification rules by primary key.
     */
    std::vector<domain::series_classification_rule>
    get_rules(const std::vector<std::string>& series_types,
              const std::vector<std::string>& metrics);

    /**
     * @brief Saves a series classification rule (creates or updates).
     *
     * @param rule The series classification rule to save.
     * @throws std::exception on failure.
     */
    void save_rule(const domain::series_classification_rule& rule);

    /**
     * @brief Saves a batch of series classification rules.
     *
     * @param rules The series classification rules to save.
     * @throws std::exception on failure.
     */
    void save_rules(const std::vector<domain::series_classification_rule>& rules);

    /**
     * @brief Deletes a series classification rule by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_rule(const std::string& series_type, const std::string& metric);

    /**
     * @brief Deletes series classification rules by their primary keys.
     */
    void delete_rules(const std::vector<std::string>& series_types,
                      const std::vector<std::string>& metrics);

    /**
     * @brief Retrieves all historical versions of a series classification rule.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::series_classification_rule> get_rule_history(const std::string& series_type,
                                                                     const std::string& metric);

private:
    context ctx_;
    repository::series_classification_rule_repository repo_;

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
    ores::utility::domain::result
    prepare_change(const messaging::series_classification_rule_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::series_classification_rule& out);
};

}

#endif
