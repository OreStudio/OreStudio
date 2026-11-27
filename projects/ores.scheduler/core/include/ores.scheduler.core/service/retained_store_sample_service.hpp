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
#ifndef ORES_SCHEDULER_CORE_SERVICE_RETAINED_STORE_SAMPLE_SERVICE_HPP
#define ORES_SCHEDULER_CORE_SERVICE_RETAINED_STORE_SAMPLE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.scheduler.api/domain/retained_store_sample.hpp"
#include "ores.scheduler.api/messaging/retained_store_sample_protocol.hpp"
#include "ores.scheduler.core/export.hpp"
#include "ores.scheduler.core/repository/retained_store_sample_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::scheduler::service {

/**
 * @brief Service for managing retained store samples.
 *
 * Provides a higher-level interface for retained store sample operations,
 * wrapping the underlying repository.
 */
class ORES_SCHEDULER_CORE_EXPORT retained_store_sample_service {
private:
    inline static std::string_view logger_name =
        "ores.scheduler.service.retained_store_sample_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a retained_store_sample_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit retained_store_sample_service(const context& ctx);

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
    ::ores::scheduler::messaging::list_retained_store_samples_response list_retained_store_samples(
        const ::ores::scheduler::messaging::list_retained_store_samples_request& request);
    ::ores::scheduler::messaging::get_retained_store_sample_response get_retained_store_sample(
        const ::ores::scheduler::messaging::get_retained_store_sample_request& request);
    ::ores::scheduler::messaging::get_many_retained_store_samples_response
    get_many_retained_store_samples(
        const ::ores::scheduler::messaging::get_many_retained_store_samples_request& request);
    ::ores::scheduler::messaging::put_retained_store_sample_response put_retained_store_sample(
        const ::ores::scheduler::messaging::put_retained_store_sample_request& request);
    ::ores::scheduler::messaging::put_many_retained_store_samples_response
    put_many_retained_store_samples(
        const ::ores::scheduler::messaging::put_many_retained_store_samples_request& request);
    ::ores::scheduler::messaging::delete_retained_store_sample_response
    delete_retained_store_sample(
        const ::ores::scheduler::messaging::delete_retained_store_sample_request& request);
    ::ores::scheduler::messaging::delete_many_retained_store_samples_response
    delete_many_retained_store_samples(
        const ::ores::scheduler::messaging::delete_many_retained_store_samples_request& request);
    /**@}*/

    /**
     * @brief Lists retained store samples with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of retained store samples for the requested page.
     */
    std::vector<domain::retained_store_sample> list_samples(std::uint32_t offset,
                                                            std::uint32_t limit);

    /**
     * @brief Gets the total count of active retained store samples.
     *
     * @return Total number of active retained store samples.
     */
    std::uint32_t count_samples();


    /**
     * @brief Retrieves a single retained store sample by its primary key.
     *
     * @return The retained store sample if found, std::nullopt otherwise.
     */
    std::optional<domain::retained_store_sample> get_sample(const std::string& id,
                                                            const std::string& sampled_at);

    /**
     * @brief Retrieves a batch of retained store samples by primary key.
     */
    std::vector<domain::retained_store_sample>
    get_samples(const std::vector<std::string>& ids, const std::vector<std::string>& sampled_ats);

    /**
     * @brief Saves a retained store sample (creates or updates).
     *
     * @param sample The retained store sample to save.
     * @throws std::exception on failure.
     */
    void save_sample(const domain::retained_store_sample& sample);

    /**
     * @brief Saves a batch of retained store samples.
     *
     * @param samples The retained store samples to save.
     * @throws std::exception on failure.
     */
    void save_samples(const std::vector<domain::retained_store_sample>& samples);

    /**
     * @brief Deletes a retained store sample by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_sample(const std::string& id, const std::string& sampled_at);

    /**
     * @brief Deletes retained store samples by their primary keys.
     */
    void delete_samples(const std::vector<std::string>& ids,
                        const std::vector<std::string>& sampled_ats);


private:
    context ctx_;
    repository::retained_store_sample_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * The caller reads the current row, so a batch reads every row it names
     * in one query rather than one query per change.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param current The row the change names as it is now, or empty.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::scheduler::messaging::retained_store_sample_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::retained_store_sample>& current,
                   domain::retained_store_sample& out);
};

}

#endif
