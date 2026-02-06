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
#include "ores.dq/service/publication_service.hpp"

#include <map>
#include <set>
#include <format>
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::dq::service {

using namespace ores::logging;
using namespace ores::database::repository;

publication_service::publication_service(context ctx)
    : ctx_(std::move(ctx)),
      dataset_repo_(ctx_),
      dependency_repo_(ctx_),
      publication_repo_(ctx_),
      artefact_type_repo_(ctx_) {
    BOOST_LOG_SEV(lg(), debug) << "publication_service initialized";
}

std::vector<domain::publication_result> publication_service::publish(
    const std::vector<boost::uuids::uuid>& dataset_ids,
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
            auto datasets = dataset_repo_.read_latest(id);
            if (!datasets.empty()) {
                ordered_datasets.push_back(datasets.front());
            } else {
                BOOST_LOG_SEV(lg(), warn) << "Dataset not found: " << id;
            }
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Publication order resolved: "
        << ordered_datasets.size() << " datasets";

    // Build artefact type cache to avoid redundant DB queries
    auto artefact_type_cache = build_artefact_type_cache(ordered_datasets);
    BOOST_LOG_SEV(lg(), debug) << "Built artefact type cache with "
        << artefact_type_cache.size() << " entries";

    // Publish each dataset in order
    std::vector<domain::publication_result> results;
    results.reserve(ordered_datasets.size());

    for (const auto& dataset : ordered_datasets) {
        BOOST_LOG_SEV(lg(), info) << "Publishing dataset: "
            << dataset.code << " (" << dataset.name << ")";

        auto result = publish_dataset(dataset, mode, artefact_type_cache);
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
            BOOST_LOG_SEV(lg(), error) << "Failed to publish " << dataset.code
                << ": " << result.error_message;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Publication complete: "
        << results.size() << " datasets processed";

    return results;
}

std::vector<domain::dataset> publication_service::resolve_publication_order(
    const std::vector<boost::uuids::uuid>& dataset_ids) {

    BOOST_LOG_SEV(lg(), debug) << "Resolving publication order for "
        << dataset_ids.size() << " datasets";

    // Fetch all datasets
    std::map<std::string, domain::dataset> datasets_by_code;
    std::map<boost::uuids::uuid, std::string> id_to_code;
    std::set<std::string> requested_codes;

    for (const auto& id : dataset_ids) {
        auto datasets = dataset_repo_.read_latest(id);
        if (!datasets.empty()) {
            const auto& dataset = datasets.front();
            datasets_by_code[dataset.code] = dataset;
            id_to_code[id] = dataset.code;
            requested_codes.insert(dataset.code);
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Dataset not found: " << id;
        }
    }

    // Fetch all dependencies
    auto all_dependencies = dependency_repo_.read_latest();
    BOOST_LOG_SEV(lg(), debug) << "Found " << all_dependencies.size()
        << " total dependencies";

    // Find all datasets that need to be included (requested + their dependencies)
    std::set<std::string> all_codes = requested_codes;
    bool changed = true;
    while (changed) {
        changed = false;
        for (const auto& dep : all_dependencies) {
            if (all_codes.count(dep.dataset_code) > 0 &&
                all_codes.count(dep.dependency_code) == 0) {
                // Need to add this dependency
                all_codes.insert(dep.dependency_code);
                changed = true;
            }
        }
    }

    // Fetch any additional datasets we need
    if (all_codes.size() > datasets_by_code.size()) {
        BOOST_LOG_SEV(lg(), debug) << "Fetching "
            << (all_codes.size() - datasets_by_code.size())
            << " additional dependency datasets";

        auto all_datasets = dataset_repo_.read_latest();
        for (const auto& ds : all_datasets) {
            if (all_codes.count(ds.code) > 0 &&
                datasets_by_code.count(ds.code) == 0) {
                datasets_by_code[ds.code] = ds;
            }
        }
    }

    // Build the dependency graph using boost.graph
    using graph_t = boost::adjacency_list<
        boost::vecS, boost::vecS, boost::directedS,
        std::string  // vertex property = dataset code
    >;

    graph_t g;
    std::map<std::string, graph_t::vertex_descriptor> code_to_vertex;

    // Add vertices
    for (const auto& code : all_codes) {
        auto v = boost::add_vertex(code, g);
        code_to_vertex[code] = v;
    }

    // Add edges (dependency -> dependent)
    // If A depends on B, edge goes from B to A (B must come before A)
    for (const auto& dep : all_dependencies) {
        if (code_to_vertex.count(dep.dataset_code) > 0 &&
            code_to_vertex.count(dep.dependency_code) > 0) {
            boost::add_edge(
                code_to_vertex[dep.dependency_code],
                code_to_vertex[dep.dataset_code],
                g);
        }
    }

    // Topological sort
    std::vector<graph_t::vertex_descriptor> sorted;
    try {
        boost::topological_sort(g, std::back_inserter(sorted));
    } catch (const boost::not_a_dag& e) {
        BOOST_LOG_SEV(lg(), error) << "Circular dependency detected in datasets";
        throw std::runtime_error("Circular dependency detected in datasets");
    }

    // Convert sorted vertices back to datasets
    // Note: topological_sort returns reverse order, so we reverse
    std::vector<domain::dataset> result;
    result.reserve(sorted.size());

    for (auto it = sorted.rbegin(); it != sorted.rend(); ++it) {
        const auto& code = g[*it];
        if (datasets_by_code.count(code) > 0) {
            result.push_back(datasets_by_code[code]);
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Publication order resolved: "
        << result.size() << " datasets";

    return result;
}

std::vector<domain::publication> publication_service::get_publication_history(
    const boost::uuids::uuid& dataset_id) {

    BOOST_LOG_SEV(lg(), debug) << "Getting publication history for dataset: "
        << dataset_id;

    return publication_repo_.read_by_dataset(dataset_id);
}

std::vector<domain::publication> publication_service::get_recent_publications(
    std::uint32_t limit) {

    BOOST_LOG_SEV(lg(), debug) << "Getting recent publications, limit: " << limit;

    return publication_repo_.read_recent(limit);
}

std::map<std::string, domain::artefact_type>
publication_service::build_artefact_type_cache(
    const std::vector<domain::dataset>& datasets) {

    BOOST_LOG_SEV(lg(), debug) << "Building artefact type cache for "
        << datasets.size() << " datasets";

    // Collect unique artefact type codes
    std::set<std::string> artefact_type_codes;
    for (const auto& dataset : datasets) {
        if (dataset.artefact_type.has_value() && !dataset.artefact_type->empty()) {
            artefact_type_codes.insert(*dataset.artefact_type);
        }
    }

    // Fetch each artefact type once
    std::map<std::string, domain::artefact_type> cache;
    for (const auto& code : artefact_type_codes) {
        auto artefact_type = artefact_type_repo_.read_by_code(code);
        if (artefact_type.has_value()) {
            cache[code] = *artefact_type;
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Artefact type not found: " << code;
        }
    }

    return cache;
}

domain::publication_result publication_service::publish_dataset(
    const domain::dataset& dataset,
    domain::publication_mode mode,
    const std::map<std::string, domain::artefact_type>& artefact_type_cache) {

    BOOST_LOG_SEV(lg(), debug) << "Publishing dataset: " << dataset.code
        << " with artefact_type: " << dataset.artefact_type.value_or("none");

    domain::publication_result result;
    result.dataset_id = dataset.id;
    result.dataset_code = dataset.code;
    result.dataset_name = dataset.name;

    if (!dataset.artefact_type.has_value() || dataset.artefact_type->empty()) {
        result.success = false;
        result.error_message = "Dataset has no artefact_type specified";
        BOOST_LOG_SEV(lg(), warn) << result.error_message
            << " for dataset: " << dataset.code;
        return result;
    }

    // Look up the artefact_type from cache
    auto it = artefact_type_cache.find(*dataset.artefact_type);
    if (it == artefact_type_cache.end()) {
        result.success = false;
        result.error_message = "Unknown artefact_type: " + *dataset.artefact_type;
        BOOST_LOG_SEV(lg(), warn) << result.error_message
            << " for dataset: " << dataset.code;
        return result;
    }

    const auto& artefact_type = it->second;

    if (!artefact_type.target_table.has_value() || artefact_type.target_table->empty()) {
        result.success = false;
        result.error_message = "Artefact type has no target_table: " + *dataset.artefact_type;
        BOOST_LOG_SEV(lg(), warn) << result.error_message
            << " for dataset: " << dataset.code;
        return result;
    }

    if (!artefact_type.populate_function.has_value() || artefact_type.populate_function->empty()) {
        result.success = false;
        result.error_message = "Artefact type has no populate_function: " + *dataset.artefact_type;
        BOOST_LOG_SEV(lg(), warn) << result.error_message
            << " for dataset: " << dataset.code;
        return result;
    }

    result.target_table = *artefact_type.target_table;
    return call_populate_function(dataset, artefact_type, mode);
}

void publication_service::record_publication(
    const domain::publication_result& result,
    domain::publication_mode mode,
    const std::string& published_by) {

    BOOST_LOG_SEV(lg(), debug) << "Recording publication for dataset: "
        << result.dataset_code;

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

domain::publication_result publication_service::call_populate_function(
    const domain::dataset& dataset,
    const domain::artefact_type& artefact_type,
    domain::publication_mode mode) {

    domain::publication_result result;
    result.dataset_id = dataset.id;
    result.dataset_code = dataset.code;
    result.dataset_name = dataset.name;
    result.target_table = *artefact_type.target_table;

    const std::string mode_str = to_string(mode);
    const std::string function_name = *artefact_type.populate_function;
    const std::string dataset_id_str = boost::uuids::to_string(dataset.id);

    BOOST_LOG_SEV(lg(), debug) << "Calling populate function: "
        << function_name << ", mode: " << mode_str;

    const auto sql = std::format(
        "SELECT * FROM ores_{}('{}'::uuid, '{}'::uuid, '{}'::text)",
        function_name, dataset_id_str, ctx_.tenant_id().to_string(), mode_str);

    try {
        auto rows = execute_raw_multi_column_query(ctx_, sql, lg(),
            std::format("Calling {}", function_name));

        // Aggregate results from the function
        // The function returns (action, record_count) rows
        for (const auto& row : rows) {
            if (row.size() >= 2 && row[0].has_value() && row[1].has_value()) {
                const std::string action = *row[0];
                const auto count = static_cast<std::uint64_t>(
                    std::stoll(*row[1]));

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

        BOOST_LOG_SEV(lg(), debug) << "Populate function completed: "
            << "inserted=" << result.records_inserted
            << ", updated=" << result.records_updated
            << ", skipped=" << result.records_skipped
            << ", deleted=" << result.records_deleted;

    } catch (const std::exception& e) {
        result.success = false;
        result.error_message = e.what();
        BOOST_LOG_SEV(lg(), error) << "Populate function failed: " << e.what();
    }

    return result;
}

messaging::publish_bundle_response publication_service::publish_bundle(
    const std::string& bundle_code,
    domain::publication_mode mode,
    const std::string& published_by,
    bool atomic) {

    BOOST_LOG_SEV(lg(), info) << "Publishing bundle: " << bundle_code
        << " with mode: " << mode
        << ", atomic: " << atomic
        << ", published_by: " << published_by;

    messaging::publish_bundle_response response;
    const std::string mode_str = to_string(mode);

    // Call the SQL function
    const auto sql = std::format(
        "SELECT * FROM ores_dq_bundles_publish_fn('{}', '{}'::uuid, '{}', '{}', {})",
        bundle_code, ctx_.tenant_id().to_string(), mode_str, published_by,
        atomic ? "true" : "false");

    try {
        auto rows = execute_raw_multi_column_query(ctx_, sql, lg(),
            std::format("Publishing bundle {}", bundle_code));

        // Parse results from the function
        // The function returns (dataset_code, dataset_name, status,
        //   records_inserted, records_updated, records_skipped, records_deleted,
        //   error_message) rows
        for (const auto& row : rows) {
            if (row.size() >= 8) {
                messaging::bundle_dataset_result dataset_result;
                dataset_result.dataset_code = row[0].value_or("");
                dataset_result.dataset_name = row[1].value_or("");
                dataset_result.status = row[2].value_or("");
                dataset_result.records_inserted = row[3].has_value() ?
                    static_cast<std::uint64_t>(std::stoll(*row[3])) : 0;
                dataset_result.records_updated = row[4].has_value() ?
                    static_cast<std::uint64_t>(std::stoll(*row[4])) : 0;
                dataset_result.records_skipped = row[5].has_value() ?
                    static_cast<std::uint64_t>(std::stoll(*row[5])) : 0;
                dataset_result.records_deleted = row[6].has_value() ?
                    static_cast<std::uint64_t>(std::stoll(*row[6])) : 0;
                dataset_result.error_message = row[7].value_or("");

                response.datasets_processed++;
                if (dataset_result.status == "success") {
                    response.datasets_succeeded++;
                    response.total_records_inserted += dataset_result.records_inserted;
                    response.total_records_updated += dataset_result.records_updated;
                    response.total_records_skipped += dataset_result.records_skipped;
                    response.total_records_deleted += dataset_result.records_deleted;
                } else if (dataset_result.status == "failed") {
                    response.datasets_failed++;
                } else if (dataset_result.status == "skipped") {
                    response.datasets_skipped++;
                }

                response.dataset_results.push_back(std::move(dataset_result));
            }
        }

        // Determine overall success
        if (atomic) {
            // In atomic mode, we would have thrown an exception if any failed
            response.success = (response.datasets_failed == 0);
        } else {
            // In non-atomic mode, success if at least one dataset succeeded
            response.success = (response.datasets_succeeded > 0);
        }

        BOOST_LOG_SEV(lg(), info) << "Bundle publication complete: "
            << response.datasets_processed << " datasets processed, "
            << response.datasets_succeeded << " succeeded, "
            << response.datasets_failed << " failed, "
            << response.datasets_skipped << " skipped";

    } catch (const std::exception& e) {
        response.success = false;
        response.error_message = e.what();
        BOOST_LOG_SEV(lg(), error) << "Bundle publication failed: " << e.what();
    }

    return response;
}

}
