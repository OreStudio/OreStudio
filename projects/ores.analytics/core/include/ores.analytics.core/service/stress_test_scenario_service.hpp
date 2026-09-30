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
#ifndef ORES_ANALYTICS_CORE_SERVICE_STRESS_TEST_SCENARIO_SERVICE_HPP
#define ORES_ANALYTICS_CORE_SERVICE_STRESS_TEST_SCENARIO_SERVICE_HPP

#include "ores.analytics.api/domain/stress_test_scenario.hpp"
#include "ores.analytics.api/messaging/stress_test_scenario_protocol.hpp"
#include "ores.analytics.core/export.hpp"
#include "ores.analytics.core/repository/stress_test_scenario_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::service {

/**
 * @brief Service for managing stress test scenarios.
 *
 * Provides a higher-level interface for stress test scenario operations,
 * wrapping the underlying repository.
 */
class ORES_ANALYTICS_CORE_EXPORT stress_test_scenario_service {
private:
    inline static std::string_view logger_name =
        "ores.analytics.service.stress_test_scenario_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a stress_test_scenario_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit stress_test_scenario_service(context ctx);

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
    messaging::list_stress_test_scenarios_response
    list_stress_test_scenarios(const messaging::list_stress_test_scenarios_request& request);
    messaging::get_stress_test_scenario_response
    get_stress_test_scenario(const messaging::get_stress_test_scenario_request& request);
    messaging::get_many_stress_test_scenarios_response get_many_stress_test_scenarios(
        const messaging::get_many_stress_test_scenarios_request& request);
    messaging::put_stress_test_scenario_response
    put_stress_test_scenario(const messaging::put_stress_test_scenario_request& request);
    messaging::put_many_stress_test_scenarios_response put_many_stress_test_scenarios(
        const messaging::put_many_stress_test_scenarios_request& request);
    messaging::delete_stress_test_scenario_response
    delete_stress_test_scenario(const messaging::delete_stress_test_scenario_request& request);
    messaging::delete_many_stress_test_scenarios_response delete_many_stress_test_scenarios(
        const messaging::delete_many_stress_test_scenarios_request& request);
    messaging::list_stress_test_scenario_versions_response list_stress_test_scenario_versions(
        const messaging::list_stress_test_scenario_versions_request& request);
    messaging::get_stress_test_scenario_version_response get_stress_test_scenario_version(
        const messaging::get_stress_test_scenario_version_request& request);
    /**@}*/

    /**
     * @brief Lists stress test scenarios with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of stress test scenarios for the requested page.
     */
    std::vector<domain::stress_test_scenario> list_stress_test_scenarios(std::uint32_t offset,
                                                                         std::uint32_t limit);

    /**
     * @brief Gets the total count of active stress test scenarios.
     *
     * @return Total number of active stress test scenarios.
     */
    std::uint32_t count_stress_test_scenarios();


    /**
     * @brief Retrieves a single stress test scenario as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The stress test scenario at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::stress_test_scenario>
    get_stress_test_scenario_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single stress test scenario by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The stress test scenario if found, std::nullopt otherwise.
     */
    std::optional<domain::stress_test_scenario>
    get_stress_test_scenario(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of stress test scenarios by primary key.
     */
    std::vector<domain::stress_test_scenario>
    get_stress_test_scenarios(const std::vector<std::string>& ids);

    /**
     * @brief Saves a stress test scenario (creates or updates).
     *
     * @param stress_test_scenario The stress test scenario to save.
     * @throws std::exception on failure.
     */
    void save_stress_test_scenario(const domain::stress_test_scenario& stress_test_scenario);

    /**
     * @brief Saves a batch of stress test scenarios.
     *
     * @param stress_test_scenarios The stress test scenarios to save.
     * @throws std::exception on failure.
     */
    void save_stress_test_scenarios(
        const std::vector<domain::stress_test_scenario>& stress_test_scenarios);

    /**
     * @brief Deletes a stress test scenario by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_stress_test_scenario(const boost::uuids::uuid& id);

    /**
     * @brief Deletes stress test scenarios by their primary keys.
     */
    void delete_stress_test_scenarios(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a stress test scenario.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::stress_test_scenario>
    get_stress_test_scenario_history(const std::string& id);

private:
    context ctx_;
    repository::stress_test_scenario_repository repo_;

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
    prepare_change(const messaging::stress_test_scenario_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::stress_test_scenario& out);
};

}

#endif
