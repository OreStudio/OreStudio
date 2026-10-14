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
#ifndef ORES_REFDATA_CORE_SERVICE_BENCHMARK_RATE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_BENCHMARK_RATE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/benchmark_rate.hpp"
#include "ores.refdata.api/messaging/benchmark_rate_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/benchmark_rate_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing benchmark rates.
 *
 * Provides a higher-level interface for benchmark rate operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT benchmark_rate_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.benchmark_rate_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a benchmark_rate_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit benchmark_rate_service(context ctx);

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
    messaging::list_benchmark_rates_response
    list_benchmark_rates(const messaging::list_benchmark_rates_request& request);
    messaging::get_benchmark_rate_response
    get_benchmark_rate(const messaging::get_benchmark_rate_request& request);
    messaging::get_many_benchmark_rates_response
    get_many_benchmark_rates(const messaging::get_many_benchmark_rates_request& request);
    messaging::put_benchmark_rate_response
    put_benchmark_rate(const messaging::put_benchmark_rate_request& request);
    messaging::put_many_benchmark_rates_response
    put_many_benchmark_rates(const messaging::put_many_benchmark_rates_request& request);
    messaging::delete_benchmark_rate_response
    delete_benchmark_rate(const messaging::delete_benchmark_rate_request& request);
    messaging::delete_many_benchmark_rates_response
    delete_many_benchmark_rates(const messaging::delete_many_benchmark_rates_request& request);
    messaging::list_benchmark_rate_versions_response
    list_benchmark_rate_versions(const messaging::list_benchmark_rate_versions_request& request);
    messaging::get_benchmark_rate_version_response
    get_benchmark_rate_version(const messaging::get_benchmark_rate_version_request& request);
    /**@}*/

    /**
     * @brief Lists benchmark rates with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of benchmark rates for the requested page.
     */
    std::vector<domain::benchmark_rate> list_rates(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active benchmark rates.
     *
     * @return Total number of active benchmark rates.
     */
    std::uint32_t count_rates();


    /**
     * @brief Retrieves a single benchmark rate as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The benchmark rate at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::benchmark_rate> get_rate_at_version(const std::string& code,
                                                              const std::string& coding_scheme_code,
                                                              std::uint32_t version);

    /**
     * @brief Retrieves a single benchmark rate by its primary key.
     *
     * @return The benchmark rate if found, std::nullopt otherwise.
     */
    std::optional<domain::benchmark_rate> get_rate(const std::string& code,
                                                   const std::string& coding_scheme_code);

    /**
     * @brief Retrieves a batch of benchmark rates by primary key.
     */
    std::vector<domain::benchmark_rate>
    get_rates(const std::vector<std::string>& codes,
              const std::vector<std::string>& coding_scheme_codes);

    /**
     * @brief Saves a benchmark rate (creates or updates).
     *
     * @param rate The benchmark rate to save.
     * @throws std::exception on failure.
     */
    void save_rate(const domain::benchmark_rate& rate);

    /**
     * @brief Saves a batch of benchmark rates.
     *
     * @param rates The benchmark rates to save.
     * @throws std::exception on failure.
     */
    void save_rates(const std::vector<domain::benchmark_rate>& rates);

    /**
     * @brief Deletes a benchmark rate by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_rate(const std::string& code, const std::string& coding_scheme_code);

    /**
     * @brief Deletes benchmark rates by their primary keys.
     */
    void delete_rates(const std::vector<std::string>& codes,
                      const std::vector<std::string>& coding_scheme_codes);

    /**
     * @brief Retrieves all historical versions of a benchmark rate.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::benchmark_rate> get_rate_history(const std::string& code,
                                                         const std::string& coding_scheme_code);

private:
    context ctx_;
    repository::benchmark_rate_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::benchmark_rate_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::benchmark_rate& out);
};

}

#endif
