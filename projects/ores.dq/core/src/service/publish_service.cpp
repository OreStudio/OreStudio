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
#include "ores.dq.core/service/publish_service.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <format>
#include <map>
#include <set>
#include <stdexcept>
#include <utility>

namespace ores::dq::service {

using namespace ores::logging;
using namespace ores::database::repository;

std::vector<std::string>
publication_order(const std::vector<std::string>& requested,
                  const std::vector<domain::dataset_dependency>& dependencies) {
    std::map<std::string, std::vector<std::string>> needs;
    for (const auto& dep : dependencies)
        needs[dep.dataset_code].push_back(dep.dependency_code);

    /*
     * A dataset's rank is the position of the first requested dataset that is it
     * or needs it, directly or not, so a dependency is published just before the
     * first dataset that reads it rather than wherever it happens to fall.
     */
    std::map<std::string, std::size_t> rank;
    for (std::size_t i = 0; i < requested.size(); ++i) {
        std::vector<std::string> pending{requested[i]};
        while (!pending.empty()) {
            const auto code = pending.back();
            pending.pop_back();
            if (!rank.emplace(code, i).second)
                continue;
            if (const auto it = needs.find(code); it != needs.end())
                pending.insert(pending.end(), it->second.begin(), it->second.end());
        }
    }

    std::map<std::string, std::size_t> unmet;
    std::map<std::string, std::vector<std::string>> needed_by;
    for (const auto& [code, r] : rank) {
        unmet[code];
        if (const auto it = needs.find(code); it != needs.end())
            for (const auto& dependency : std::set(it->second.begin(), it->second.end())) {
                ++unmet[code];
                needed_by[dependency].push_back(code);
            }
    }

    std::set<std::pair<std::size_t, std::string>> ready;
    for (const auto& [code, count] : unmet)
        if (count == 0)
            ready.emplace(rank.at(code), code);

    std::vector<std::string> order;
    order.reserve(rank.size());
    while (!ready.empty()) {
        const auto code = ready.begin()->second;
        ready.erase(ready.begin());
        order.push_back(code);
        if (const auto it = needed_by.find(code); it != needed_by.end())
            for (const auto& dependent : it->second)
                if (--unmet.at(dependent) == 0)
                    ready.emplace(rank.at(dependent), dependent);
    }

    if (order.size() != rank.size())
        throw std::runtime_error("Circular dependency detected in datasets");
    return order;
}

publish_service::publish_service(context ctx)
    : ctx_(std::move(ctx))
    , dependency_repo_(ctx_) {
    BOOST_LOG_SEV(lg(), debug) << "publish_service initialized";
}

std::vector<domain::publication_result>
publish_service::publish(const std::vector<boost::uuids::uuid>& dataset_ids,
                         domain::publication_mode mode,
                         const std::string& published_by,
                         bool resolve_dependencies) {

    BOOST_LOG_SEV(lg(), info) << "Publishing " << dataset_ids.size()
                              << " datasets with mode: " << mode
                              << ", resolve_dependencies: " << resolve_dependencies
                              << ", published_by: " << published_by;

    if (dataset_ids.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No datasets specified for publication";
        return {};
    }

    // Resolve publication order (handles dependencies if requested)
    std::vector<domain::dataset> ordered_datasets;
    if (resolve_dependencies) {
        ordered_datasets = resolve_publication_order(dataset_ids);
    } else {
        // Just fetch the datasets in the order given
        for (const auto& id : dataset_ids) {
            auto datasets = dataset_repo_.read_latest(ctx_, boost::uuids::to_string(id));
            if (!datasets.empty()) {
                ordered_datasets.push_back(datasets.front());
            } else {
                BOOST_LOG_SEV(lg(), warn) << "Dataset not found: " << id;
            }
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Publication order resolved: " << ordered_datasets.size()
                              << " datasets";

    // Build artefact type cache to avoid redundant DB queries
    auto artefact_type_cache = build_artefact_type_cache(ordered_datasets);
    BOOST_LOG_SEV(lg(), debug) << "Built artefact type cache with " << artefact_type_cache.size()
                               << " entries";

    // Publish each dataset in order
    std::vector<domain::publication_result> results;
    results.reserve(ordered_datasets.size());

    for (const auto& dataset : ordered_datasets) {
        BOOST_LOG_SEV(lg(), info) << "Publishing dataset: " << dataset.code << " (" << dataset.name
                                  << ")";

        auto result = publish_dataset(dataset, mode, published_by, artefact_type_cache);
        results.push_back(result);

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Successfully published " << dataset.code
                                      << " - inserted: " << result.records_inserted
                                      << ", updated: " << result.records_updated
                                      << ", skipped: " << result.records_skipped
                                      << ", deleted: " << result.records_deleted;

            // Record in audit table
            record_publication(result, mode, published_by);
        } else {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to publish " << dataset.code << ": " << result.error_message;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Publication complete: " << results.size()
                              << " datasets processed";

    return results;
}

std::vector<domain::dataset>
publish_service::resolve_publication_order(const std::vector<boost::uuids::uuid>& dataset_ids) {

    BOOST_LOG_SEV(lg(), debug) << "Resolving publication order for " << dataset_ids.size()
                               << " datasets";

    std::map<std::string, domain::dataset> datasets_by_code;
    std::vector<std::string> requested;
    requested.reserve(dataset_ids.size());
    for (const auto& id : dataset_ids) {
        auto datasets = dataset_repo_.read_latest(ctx_, boost::uuids::to_string(id));
        if (datasets.empty()) {
            BOOST_LOG_SEV(lg(), warn) << "Dataset not found: " << id;
            continue;
        }
        const auto& dataset = datasets.front();
        if (datasets_by_code.emplace(dataset.code, dataset).second)
            requested.push_back(dataset.code);
    }

    const auto all_dependencies = dependency_repo_.read_latest();
    BOOST_LOG_SEV(lg(), debug) << "Found " << all_dependencies.size() << " total dependencies";

    std::vector<std::string> codes;
    try {
        codes = publication_order(requested, all_dependencies);
    } catch (const std::runtime_error& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        throw;
    }

    if (codes.size() > datasets_by_code.size()) {
        BOOST_LOG_SEV(lg(), debug) << "Fetching " << (codes.size() - datasets_by_code.size())
                                   << " additional dependency datasets";
        for (const auto& ds : dataset_repo_.read_latest(ctx_))
            datasets_by_code.emplace(ds.code, ds);
    }

    std::vector<domain::dataset> result;
    result.reserve(codes.size());
    for (const auto& code : codes) {
        const auto it = datasets_by_code.find(code);
        if (it != datasets_by_code.end())
            result.push_back(it->second);
    }

    BOOST_LOG_SEV(lg(), debug) << "Publication order resolved: " << result.size() << " datasets";

    return result;
}


std::map<std::string, domain::artefact_type>
publish_service::build_artefact_type_cache(const std::vector<domain::dataset>& datasets) {

    BOOST_LOG_SEV(lg(), debug) << "Building artefact type cache for " << datasets.size()
                               << " datasets";

    // Collect unique artefact type codes
    std::set<std::string> artefact_type_codes;
    for (const auto& dataset : datasets) {
        if (!dataset.artefact_type.empty())
            artefact_type_codes.insert(dataset.artefact_type);
    }

    // Fetch each artefact type once
    std::map<std::string, domain::artefact_type> cache;
    for (const auto& code : artefact_type_codes) {
        auto artefact_types = artefact_type_repo_.read_latest(ctx_, code);
        if (!artefact_types.empty()) {
            cache[code] = std::move(artefact_types.front());
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Artefact type not found: " << code;
        }
    }

    return cache;
}

domain::publication_result publish_service::publish_dataset(
    const domain::dataset& dataset,
    domain::publication_mode mode,
    const std::string& published_by,
    const std::map<std::string, domain::artefact_type>& artefact_type_cache) {

    BOOST_LOG_SEV(lg(), debug) << "Publishing dataset: " << dataset.code
                               << " with artefact_type: " << dataset.artefact_type;

    domain::publication_result result;
    result.dataset_id = dataset.id;
    result.dataset_code = dataset.code;
    result.dataset_name = dataset.name;

    if (dataset.artefact_type.empty()) {
        result.success = false;
        result.error_message = "Dataset has no artefact_type specified";
        BOOST_LOG_SEV(lg(), warn) << result.error_message << " for dataset: " << dataset.code;
        return result;
    }

    // Look up the artefact_type from cache
    auto it = artefact_type_cache.find(dataset.artefact_type);
    if (it == artefact_type_cache.end()) {
        result.success = false;
        result.error_message = "Unknown artefact_type: " + dataset.artefact_type;
        BOOST_LOG_SEV(lg(), warn) << result.error_message << " for dataset: " << dataset.code;
        return result;
    }

    const auto& artefact_type = it->second;

    if (!artefact_type.target_table.has_value() || artefact_type.target_table->empty()) {
        result.success = false;
        result.error_message = "Artefact type has no target_table: " + dataset.artefact_type;
        BOOST_LOG_SEV(lg(), warn) << result.error_message << " for dataset: " << dataset.code;
        return result;
    }

    if (!artefact_type.target_subject.has_value() || artefact_type.target_subject->empty()) {
        result.success = false;
        result.error_message = "Artefact type has no target_subject: " + dataset.artefact_type;
        BOOST_LOG_SEV(lg(), warn) << result.error_message << " for dataset: " << dataset.code;
        return result;
    }

    result.target_table = *artefact_type.target_table;
    return call_populate_function(dataset, artefact_type, mode, published_by);
}

void publish_service::record_publication(const domain::publication_result& result,
                                         domain::publication_mode mode,
                                         const std::string& published_by) {

    BOOST_LOG_SEV(lg(), debug) << "Recording publication for dataset: " << result.dataset_code;

    // Build the INSERT query
    const auto sql = std::format(
        "INSERT INTO ores_dq_dataset_publications_tbl ("
        "tenant_id, dataset_id, dataset_code, mode, target_table, "
        "records_inserted, records_updated, records_skipped, records_deleted, published_by"
        ") VALUES ('{}', '{}', '{}', '{}', '{}', {}, {}, {}, {}, '{}')",
        ctx_.tenant_id().to_string(),
        boost::uuids::to_string(result.dataset_id),
        result.dataset_code,
        to_string(mode),
        result.target_table,
        result.records_inserted,
        result.records_updated,
        result.records_skipped,
        result.records_deleted,
        published_by);

    try {
        execute_raw_command(ctx_, sql, lg(), "Recording publication");
        BOOST_LOG_SEV(lg(), debug) << "Publication recorded successfully";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to record publication: " << e.what();
    }
}

domain::publication_result
publish_service::call_populate_function(const domain::dataset& dataset,
                                        const domain::artefact_type& artefact_type,
                                        domain::publication_mode mode,
                                        const std::string& /*published_by*/) {

    domain::publication_result result;
    result.dataset_id = dataset.id;
    result.dataset_code = dataset.code;
    result.dataset_name = dataset.name;
    result.target_table = *artefact_type.target_table;

    const std::string mode_str = to_string(mode);
    const std::string function_name = *artefact_type.target_subject;
    const std::string dataset_id_str = boost::uuids::to_string(dataset.id);

    BOOST_LOG_SEV(lg(), debug) << "Calling populate function: " << function_name
                               << ", mode: " << mode_str;

    const auto sql = std::format("SELECT * FROM {}('{}'::uuid, '{}'::uuid, '{}'::text)",
                                 function_name,
                                 dataset_id_str,
                                 ctx_.tenant_id().to_string(),
                                 mode_str);

    try {
        auto rows = execute_raw_multi_column_query(
            ctx_, sql, lg(), std::format("Calling {}", function_name));

        // Aggregate results from the function
        // The function returns (action, record_count) rows
        for (const auto& row : rows) {
            if (row.size() >= 2 && row[0].has_value() && row[1].has_value()) {
                const std::string action = *row[0];
                const auto count = static_cast<std::uint64_t>(std::stoll(*row[1]));

                if (action == "inserted") {
                    result.records_inserted += count;
                } else if (action == "updated") {
                    result.records_updated += count;
                } else if (action == "skipped") {
                    result.records_skipped += count;
                } else if (action == "deleted") {
                    result.records_deleted += count;
                }
            }
        }

        result.success = true;

        BOOST_LOG_SEV(lg(), debug)
            << "Populate function completed: " << "inserted=" << result.records_inserted
            << ", updated=" << result.records_updated << ", skipped=" << result.records_skipped
            << ", deleted=" << result.records_deleted;

    } catch (const std::exception& e) {
        result.success = false;
        result.error_message = e.what();
        BOOST_LOG_SEV(lg(), error) << "Populate function failed: " << e.what();
    }

    return result;
}

std::vector<bundle_publishable_dataset>
publish_service::list_publishable_datasets(const std::vector<boost::uuids::uuid>& dataset_ids,
                                           bool resolve_dependencies) {

    BOOST_LOG_SEV(lg(), debug) << "Resolving publishable datasets for " << dataset_ids.size()
                               << " IDs, resolve_dependencies=" << resolve_dependencies;

    std::vector<domain::dataset> ordered_datasets;
    if (resolve_dependencies) {
        ordered_datasets = resolve_publication_order(dataset_ids);
    } else {
        for (const auto& id : dataset_ids) {
            auto rows = dataset_repo_.read_latest(ctx_, boost::uuids::to_string(id));
            if (!rows.empty())
                ordered_datasets.push_back(rows.front());
            else
                BOOST_LOG_SEV(lg(), warn) << "Dataset not found: " << id;
        }
    }

    auto cache = build_artefact_type_cache(ordered_datasets);

    std::vector<bundle_publishable_dataset> result;
    result.reserve(ordered_datasets.size());
    for (const auto& ds : ordered_datasets) {
        if (ds.artefact_type.empty()) {
            BOOST_LOG_SEV(lg(), warn) << "Skipping dataset without artefact_type: " << ds.code;
            continue;
        }
        auto it = cache.find(ds.artefact_type);
        if (it == cache.end()) {
            BOOST_LOG_SEV(lg(), warn) << "Artefact type not found for dataset: " << ds.code;
            continue;
        }
        if (!it->second.target_subject.has_value() || it->second.target_subject->empty()) {
            BOOST_LOG_SEV(lg(), warn) << "No target_subject for dataset: " << ds.code;
            continue;
        }
        bundle_publishable_dataset entry;
        entry.dataset_id = boost::uuids::to_string(ds.id);
        entry.dataset_code = ds.code;
        entry.target_subject = *it->second.target_subject;
        result.push_back(std::move(entry));
    }

    BOOST_LOG_SEV(lg(), debug) << "Resolved " << result.size() << " publishable datasets";
    return result;
}

std::vector<bundle_publishable_dataset>
publish_service::list_bundle_publishable_datasets(const std::string& bundle_code,
                                                  bool resolve_dependencies) {

    BOOST_LOG_SEV(lg(), debug) << "Listing publishable datasets for bundle: " << bundle_code
                               << ", resolve_dependencies=" << resolve_dependencies;

    // Direct member IDs + their optional flag -- ores_dq_bundle_datasets_list_fn already does
    // the dataset_code -> dataset_id lookup (skipping codes with no matching dataset record); the
    // dependency closure and target_subject/is_publishable filtering both happen in
    // list_publishable_datasets() below, the same core the ID-based publish path uses (it has no
    // notion of "optional" -- that's bundle-membership metadata, not a dataset property -- so the
    // flag is merged back in afterwards by dataset_id).
    const auto sql = "SELECT dataset_id, optional FROM ores_dq_bundle_datasets_list_fn($1) "
                     "WHERE dataset_id IS NOT NULL ORDER BY display_order";

    auto rows = execute_parameterized_multi_column_query(
        ctx_, sql, {bundle_code}, lg(), std::format("Listing bundle members for {}", bundle_code));

    std::vector<boost::uuids::uuid> member_ids;
    std::map<boost::uuids::uuid, bool> optional_by_id;
    member_ids.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() >= 2 && row[0].has_value()) {
            try {
                auto id = boost::lexical_cast<boost::uuids::uuid>(*row[0]);
                member_ids.push_back(id);
                optional_by_id[id] = database::repository::text_to_bool(row[1]);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Invalid dataset_id in bundle " << bundle_code << ": " << e.what();
            }
        }
    }

    auto result = list_publishable_datasets(member_ids, resolve_dependencies);
    for (auto& entry : result) {
        try {
            auto it =
                optional_by_id.find(boost::lexical_cast<boost::uuids::uuid>(entry.dataset_id));
            entry.optional = it != optional_by_id.end() && it->second;
        } catch (const std::exception&) {
            entry.optional = false;
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Found " << result.size() << " publishable datasets in bundle "
                               << bundle_code << " (" << member_ids.size() << " direct member(s))";

    return result;
}

}
