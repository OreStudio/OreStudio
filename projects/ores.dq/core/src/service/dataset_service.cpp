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
#include "ores.dq.core/service/dataset_service.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>

namespace ores::dq::service {

using namespace ores::logging;

dataset_service::dataset_service(context ctx)
    : dataset_repo_(ctx) {}

// ============================================================================
// Dataset Management
// ============================================================================

std::vector<domain::dataset> dataset_service::list_datasets() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all datasets";
    return dataset_repo_.read_latest();
}

std::vector<domain::dataset> dataset_service::list_datasets(std::uint32_t offset,
                                                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing datasets with pagination: offset=" << offset
                               << ", limit=" << limit;
    return dataset_repo_.read_latest(offset, limit);
}

std::uint32_t dataset_service::get_dataset_count() {
    return dataset_repo_.get_total_count();
}

std::optional<domain::dataset> dataset_service::find_dataset(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding dataset: " << id;
    auto datasets = dataset_repo_.read_latest(id);
    if (datasets.empty()) {
        return std::nullopt;
    }
    return datasets.front();
}

void dataset_service::save_dataset(const domain::dataset& dataset) {
    if (dataset.id.is_nil()) {
        throw std::invalid_argument("Dataset ID cannot be nil.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving dataset: " << dataset.id;
    dataset_repo_.write(dataset);
    BOOST_LOG_SEV(lg(), info) << "Saved dataset: " << dataset.id;
}

void dataset_service::save_datasets(const std::vector<domain::dataset>& datasets) {
    for (const auto& d : datasets) {
        if (d.id.is_nil()) {
            throw std::invalid_argument("Dataset ID cannot be nil.");
        }
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << datasets.size() << " datasets";
    dataset_repo_.write(datasets);
}

void dataset_service::remove_dataset(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing dataset: " << id;
    dataset_repo_.remove(id);
    BOOST_LOG_SEV(lg(), info) << "Removed dataset: " << id;
}

std::vector<domain::dataset> dataset_service::get_dataset_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for dataset: " << id;
    return dataset_repo_.read_all(id);
}

}
