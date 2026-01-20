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
      publication_repo_(ctx_) {
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

    // Publish each dataset in order
    std::vector<domain::publication_result> results;
    results.reserve(ordered_datasets.size());

    for (const auto& dataset : ordered_datasets) {
        BOOST_LOG_SEV(lg(), info) << "Publishing dataset: "
            << dataset.code << " (" << dataset.name << ")";

        auto result = publish_dataset(dataset, mode);
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

domain::publication_result publication_service::publish_dataset(
    const domain::dataset& dataset,
    domain::publication_mode mode) {

    BOOST_LOG_SEV(lg(), debug) << "Publishing dataset: " << dataset.code
        << " with artefact_type: "
        << (dataset.artefact_type.value_or("none"));

    domain::publication_result result;
    result.dataset_id = dataset.id;
    result.dataset_code = dataset.code;
    result.dataset_name = dataset.name;

    if (!dataset.artefact_type.has_value()) {
        result.success = false;
        result.error_message = "Dataset has no artefact_type specified";
        BOOST_LOG_SEV(lg(), warn) << result.error_message
            << " for dataset: " << dataset.code;
        return result;
    }

    result.target_table = get_target_table(*dataset.artefact_type);
    if (result.target_table.empty()) {
        result.success = false;
        result.error_message = "Unknown artefact_type: " + *dataset.artefact_type;
        BOOST_LOG_SEV(lg(), error) << result.error_message
            << " for dataset: " << dataset.code;
        return result;
    }

    return call_populate_function(dataset, mode);
}

void publication_service::record_publication(
    const domain::publication_result& result,
    domain::publication_mode mode,
    const std::string& published_by) {

    BOOST_LOG_SEV(lg(), debug) << "Recording publication for dataset: "
        << result.dataset_code;

    // Build the INSERT query
    const auto sql = std::format(
        "INSERT INTO ores.dq_publications_tbl ("
        "dataset_id, dataset_code, mode, target_table, "
        "records_inserted, records_updated, records_skipped, records_deleted, published_by"
        ") VALUES ('{}', '{}', '{}', '{}', {}, {}, {}, {}, '{}')",
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
    domain::publication_mode mode) {

    domain::publication_result result;
    result.dataset_id = dataset.id;
    result.dataset_code = dataset.code;
    result.dataset_name = dataset.name;
    result.target_table = get_target_table(*dataset.artefact_type);

    const std::string mode_str = to_string(mode);
    const std::string artefact_type = *dataset.artefact_type;
    const std::string dataset_id_str = boost::uuids::to_string(dataset.id);

    BOOST_LOG_SEV(lg(), debug) << "Calling populate function for artefact_type: "
        << artefact_type << ", mode: " << mode_str;

    std::string function_name;
    if (artefact_type == "images") {
        function_name = "dq_populate_images";
    } else if (artefact_type == "countries") {
        function_name = "dq_populate_countries";
    } else if (artefact_type == "currencies") {
        function_name = "dq_populate_currencies";
    } else if (artefact_type == "ip2country") {
        function_name = "dq_populate_ip2country";
    } else {
        result.success = false;
        result.error_message = "Unknown artefact_type: " + artefact_type;
        return result;
    }

    const auto sql = std::format(
        "SELECT * FROM ores.{}('{}', '{}')",
        function_name, dataset_id_str, mode_str);

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

std::string publication_service::get_target_table(const std::string& artefact_type) {
    if (artefact_type == "images") {
        return "assets_images_tbl";
    } else if (artefact_type == "countries") {
        return "refdata_countries_tbl";
    } else if (artefact_type == "currencies") {
        return "refdata_currencies_tbl";
    } else if (artefact_type == "ip2country") {
        return "geo_ip2country_tbl";
    }
    return "";
}

}
