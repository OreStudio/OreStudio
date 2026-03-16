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
#include <span>
#include <string>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.dq/messaging/dataset_protocol.hpp"
#include "ores.dq/messaging/dataset_bundle_protocol.hpp"
#include "ores.dq/messaging/publication_protocol.hpp"
#include "ores.dq/messaging/coding_scheme_protocol.hpp"
#include "ores.dq/service/data_organization_service.hpp"
#include "ores.dq/service/dimension_service.hpp"
#include "ores.dq/service/dataset_service.hpp"
#include "ores.dq/service/dataset_bundle_service.hpp"
#include "ores.dq/service/publication_service.hpp"
#include "ores.dq/service/coding_scheme_service.hpp"
#include "ores.dq/messaging/registrar.hpp"

namespace {

template<typename Resp>
void reply(ores::nats::service::client& nats, const ores::nats::message& msg,
    const Resp& resp) {
    if (msg.reply_subject.empty()) return;
    const auto json = rfl::json::write(resp);
    const auto* p = reinterpret_cast<const std::byte*>(json.data());
    nats.publish(msg.reply_subject, std::span<const std::byte>(p, json.size()));
}

template<typename Req>
std::optional<Req> decode(const ores::nats::message& msg) {
    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto r = rfl::json::read<Req>(sv);
    if (!r) return std::nullopt;
    return *r;
}

} // anonymous namespace

namespace ores::dq::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    context_extractor_fn context_extractor) {
    std::vector<ores::nats::service::subscription> subs;

    subs.push_back(nats.queue_subscribe(
        "dq.v1.>", "ores.dq.service",
        [&nats, base_ctx = ctx, context_extractor](ores::nats::message msg) mutable {
            auto ctx = (context_extractor ?
                context_extractor(msg) : std::nullopt).value_or(base_ctx);

            // ==================================================================
            // Catalogs
            // ==================================================================

            if (msg.subject.ends_with("catalogs.list")) {
                auto req = decode<get_catalogs_request>(msg);
                if (!req) return;
                service::data_organization_service svc(ctx);
                try {
                    const auto items = svc.list_catalogs(
                        static_cast<std::uint32_t>(req->offset),
                        static_cast<std::uint32_t>(req->limit));
                    const auto count = svc.get_catalog_count();
                    get_catalogs_response resp;
                    resp.catalogs = items;
                    resp.total_available_count = static_cast<int>(count);
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_catalogs_response resp;
                    resp.total_available_count = 0;
                    reply(nats, msg, resp);
                }
                return;
            }

            if (msg.subject.ends_with("catalogs.save")) {
                auto req = decode<save_catalog_request>(msg);
                if (!req) return;
                service::data_organization_service svc(ctx);
                try {
                    svc.save_catalog(req->data);
                    reply(nats, msg, save_catalog_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg, save_catalog_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("catalogs.delete")) {
                auto req = decode<delete_catalog_request>(msg);
                if (!req) return;
                service::data_organization_service svc(ctx);
                try {
                    for (const auto& code : req->codes)
                        svc.remove_catalog(code);
                    reply(nats, msg, delete_catalog_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg, delete_catalog_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("catalogs.history")) {
                auto req = decode<get_catalog_history_request>(msg);
                if (!req) return;
                service::data_organization_service svc(ctx);
                try {
                    const auto history = svc.get_catalog_history(req->code);
                    get_catalog_history_response resp;
                    resp.success = true;
                    resp.history = history;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_catalog_history_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }

            // ==================================================================
            // Data Domains
            // ==================================================================

            if (msg.subject.ends_with("data-domains.list")) {
                auto req = decode<get_data_domains_request>(msg);
                if (!req) return;
                service::data_organization_service svc(ctx);
                try {
                    const auto items = svc.list_data_domains();
                    get_data_domains_response resp;
                    resp.domains = items;
                    resp.total_available_count = static_cast<int>(items.size());
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_data_domains_response resp;
                    resp.total_available_count = 0;
                    reply(nats, msg, resp);
                }
                return;
            }

            if (msg.subject.ends_with("data-domains.save")) {
                auto req = decode<save_data_domain_request>(msg);
                if (!req) return;
                service::data_organization_service svc(ctx);
                try {
                    svc.save_data_domain(req->data);
                    reply(nats, msg, save_data_domain_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg, save_data_domain_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("data-domains.delete")) {
                auto req = decode<delete_data_domain_request>(msg);
                if (!req) return;
                service::data_organization_service svc(ctx);
                try {
                    for (const auto& name : req->names)
                        svc.remove_data_domain(name);
                    reply(nats, msg, delete_data_domain_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        delete_data_domain_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("data-domains.history")) {
                auto req = decode<get_data_domain_history_request>(msg);
                if (!req) return;
                service::data_organization_service svc(ctx);
                try {
                    const auto history =
                        svc.get_data_domain_history(req->name);
                    get_data_domain_history_response resp;
                    resp.success = true;
                    resp.history = history;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_data_domain_history_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }

            // ==================================================================
            // Methodologies
            // ==================================================================

            if (msg.subject.ends_with("methodologies.list")) {
                auto req = decode<get_methodologies_request>(msg);
                if (!req) return;
                service::dataset_service svc(ctx);
                try {
                    const auto items = svc.list_methodologies(
                        static_cast<std::uint32_t>(req->offset),
                        static_cast<std::uint32_t>(req->limit));
                    const auto count = svc.get_methodology_count();
                    get_methodologies_response resp;
                    resp.methodologies = items;
                    resp.total_available_count = static_cast<int>(count);
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_methodologies_response resp;
                    resp.total_available_count = 0;
                    reply(nats, msg, resp);
                }
                return;
            }

            if (msg.subject.ends_with("methodologies.save")) {
                auto req = decode<save_methodology_request>(msg);
                if (!req) return;
                service::dataset_service svc(ctx);
                try {
                    svc.save_methodology(req->data);
                    reply(nats, msg, save_methodology_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        save_methodology_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("methodologies.delete")) {
                auto req = decode<delete_methodology_request>(msg);
                if (!req) return;
                service::dataset_service svc(ctx);
                try {
                    for (const auto& code : req->codes)
                        svc.remove_methodology(
                            boost::uuids::string_generator{}(code));
                    reply(nats, msg, delete_methodology_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        delete_methodology_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("methodologies.history")) {
                auto req = decode<get_methodology_history_request>(msg);
                if (!req) return;
                service::dataset_service svc(ctx);
                try {
                    const auto history = svc.get_methodology_history(
                        boost::uuids::string_generator{}(req->code));
                    get_methodology_history_response resp;
                    resp.success = true;
                    resp.history = history;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_methodology_history_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }

            // ==================================================================
            // Subject Areas
            // ==================================================================

            if (msg.subject.ends_with("subject-areas.list")) {
                auto req = decode<get_subject_areas_request>(msg);
                if (!req) return;
                service::data_organization_service svc(ctx);
                try {
                    const auto items = svc.list_subject_areas(
                        static_cast<std::uint32_t>(req->offset),
                        static_cast<std::uint32_t>(req->limit));
                    const auto count = svc.get_subject_area_count();
                    get_subject_areas_response resp;
                    resp.subject_areas = items;
                    resp.total_available_count = static_cast<int>(count);
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_subject_areas_response resp;
                    resp.total_available_count = 0;
                    reply(nats, msg, resp);
                }
                return;
            }

            if (msg.subject.ends_with("subject-areas.save")) {
                auto req = decode<save_subject_area_request>(msg);
                if (!req) return;
                service::data_organization_service svc(ctx);
                try {
                    svc.save_subject_area(req->data);
                    reply(nats, msg, save_subject_area_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        save_subject_area_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("subject-areas.delete")) {
                auto req = decode<delete_subject_area_request>(msg);
                if (!req) return;
                service::data_organization_service svc(ctx);
                try {
                    for (const auto& key : req->keys)
                        svc.remove_subject_area(key.name, key.domain_name);
                    reply(nats, msg, delete_subject_area_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        delete_subject_area_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("subject-areas.history")) {
                auto req = decode<get_subject_area_history_request>(msg);
                if (!req) return;
                service::data_organization_service svc(ctx);
                try {
                    const auto history = svc.get_subject_area_history(
                        req->key.name, req->key.domain_name);
                    get_subject_area_history_response resp;
                    resp.success = true;
                    resp.history = history;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_subject_area_history_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }

            // ==================================================================
            // Nature Dimensions
            // ==================================================================

            if (msg.subject.ends_with("nature-dimensions.list")) {
                auto req = decode<get_nature_dimensions_request>(msg);
                if (!req) return;
                service::dimension_service svc(ctx);
                try {
                    const auto items = svc.list_nature_dimensions();
                    get_nature_dimensions_response resp;
                    resp.nature_dimensions = items;
                    resp.total_available_count = static_cast<int>(items.size());
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_nature_dimensions_response resp;
                    resp.total_available_count = 0;
                    reply(nats, msg, resp);
                }
                return;
            }

            if (msg.subject.ends_with("nature-dimensions.save")) {
                auto req = decode<save_nature_dimension_request>(msg);
                if (!req) return;
                service::dimension_service svc(ctx);
                try {
                    svc.save_nature_dimension(req->data);
                    reply(nats, msg, save_nature_dimension_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        save_nature_dimension_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("nature-dimensions.delete")) {
                auto req = decode<delete_nature_dimension_request>(msg);
                if (!req) return;
                service::dimension_service svc(ctx);
                try {
                    svc.remove_nature_dimensions(req->codes);
                    reply(nats, msg,
                        delete_nature_dimension_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        delete_nature_dimension_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("nature-dimensions.history")) {
                auto req = decode<get_nature_dimension_history_request>(msg);
                if (!req) return;
                service::dimension_service svc(ctx);
                try {
                    const auto history =
                        svc.get_nature_dimension_history(req->code);
                    get_nature_dimension_history_response resp;
                    resp.success = true;
                    resp.history = history;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_nature_dimension_history_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }

            // ==================================================================
            // Origin Dimensions
            // ==================================================================

            if (msg.subject.ends_with("origin-dimensions.list")) {
                auto req = decode<get_origin_dimensions_request>(msg);
                if (!req) return;
                service::dimension_service svc(ctx);
                try {
                    const auto items = svc.list_origin_dimensions();
                    get_origin_dimensions_response resp;
                    resp.origin_dimensions = items;
                    resp.total_available_count = static_cast<int>(items.size());
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_origin_dimensions_response resp;
                    resp.total_available_count = 0;
                    reply(nats, msg, resp);
                }
                return;
            }

            if (msg.subject.ends_with("origin-dimensions.save")) {
                auto req = decode<save_origin_dimension_request>(msg);
                if (!req) return;
                service::dimension_service svc(ctx);
                try {
                    svc.save_origin_dimension(req->data);
                    reply(nats, msg, save_origin_dimension_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        save_origin_dimension_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("origin-dimensions.delete")) {
                auto req = decode<delete_origin_dimension_request>(msg);
                if (!req) return;
                service::dimension_service svc(ctx);
                try {
                    svc.remove_origin_dimensions(req->codes);
                    reply(nats, msg,
                        delete_origin_dimension_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        delete_origin_dimension_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("origin-dimensions.history")) {
                auto req = decode<get_origin_dimension_history_request>(msg);
                if (!req) return;
                service::dimension_service svc(ctx);
                try {
                    const auto history =
                        svc.get_origin_dimension_history(req->code);
                    get_origin_dimension_history_response resp;
                    resp.success = true;
                    resp.history = history;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_origin_dimension_history_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }

            // ==================================================================
            // Treatment Dimensions
            // ==================================================================

            if (msg.subject.ends_with("treatment-dimensions.list")) {
                auto req = decode<get_treatment_dimensions_request>(msg);
                if (!req) return;
                service::dimension_service svc(ctx);
                try {
                    const auto items = svc.list_treatment_dimensions();
                    get_treatment_dimensions_response resp;
                    resp.treatment_dimensions = items;
                    resp.total_available_count = static_cast<int>(items.size());
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_treatment_dimensions_response resp;
                    resp.total_available_count = 0;
                    reply(nats, msg, resp);
                }
                return;
            }

            if (msg.subject.ends_with("treatment-dimensions.save")) {
                auto req = decode<save_treatment_dimension_request>(msg);
                if (!req) return;
                service::dimension_service svc(ctx);
                try {
                    svc.save_treatment_dimension(req->data);
                    reply(nats, msg,
                        save_treatment_dimension_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        save_treatment_dimension_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("treatment-dimensions.delete")) {
                auto req = decode<delete_treatment_dimension_request>(msg);
                if (!req) return;
                service::dimension_service svc(ctx);
                try {
                    svc.remove_treatment_dimensions(req->codes);
                    reply(nats, msg,
                        delete_treatment_dimension_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        delete_treatment_dimension_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("treatment-dimensions.history")) {
                auto req =
                    decode<get_treatment_dimension_history_request>(msg);
                if (!req) return;
                service::dimension_service svc(ctx);
                try {
                    const auto history =
                        svc.get_treatment_dimension_history(req->code);
                    get_treatment_dimension_history_response resp;
                    resp.success = true;
                    resp.history = history;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_treatment_dimension_history_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }

            // ==================================================================
            // Datasets
            // ==================================================================

            if (msg.subject.ends_with("datasets.list")) {
                auto req = decode<get_datasets_request>(msg);
                if (!req) return;
                service::dataset_service svc(ctx);
                try {
                    const auto items = svc.list_datasets(
                        static_cast<std::uint32_t>(req->offset),
                        static_cast<std::uint32_t>(req->limit));
                    const auto count = svc.get_dataset_count();
                    get_datasets_response resp;
                    resp.datasets = items;
                    resp.total_available_count = static_cast<int>(count);
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_datasets_response resp;
                    resp.total_available_count = 0;
                    reply(nats, msg, resp);
                }
                return;
            }

            if (msg.subject.ends_with("datasets.save")) {
                auto req = decode<save_dataset_request>(msg);
                if (!req) return;
                service::dataset_service svc(ctx);
                try {
                    svc.save_datasets(req->datasets);
                    reply(nats, msg, save_dataset_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg, save_dataset_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("datasets.delete")) {
                auto req = decode<delete_dataset_request>(msg);
                if (!req) return;
                service::dataset_service svc(ctx);
                try {
                    boost::uuids::string_generator gen;
                    for (const auto& id : req->ids)
                        svc.remove_dataset(gen(id));
                    reply(nats, msg, delete_dataset_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        delete_dataset_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("datasets.history")) {
                auto req = decode<get_dataset_history_request>(msg);
                if (!req) return;
                service::dataset_service svc(ctx);
                try {
                    const auto history = svc.get_dataset_history(
                        boost::uuids::string_generator{}(req->id));
                    get_dataset_history_response resp;
                    resp.success = true;
                    resp.history = history;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_dataset_history_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }

            if (msg.subject.ends_with("datasets.publish")) {
                auto req = decode<publish_datasets_request>(msg);
                if (!req) return;
                service::publication_service svc(ctx);
                try {
                    boost::uuids::string_generator gen;
                    std::vector<boost::uuids::uuid> uuids;
                    uuids.reserve(req->dataset_ids.size());
                    for (const auto& id : req->dataset_ids)
                        uuids.push_back(gen(id));
                    const auto results = svc.publish(
                        uuids, req->mode, req->published_by,
                        req->resolve_dependencies);
                    publish_datasets_response resp;
                    resp.success = true;
                    resp.results = results;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    publish_datasets_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }

            // ==================================================================
            // Dataset Bundles
            // ==================================================================

            if (msg.subject.ends_with("dataset-bundles.list")) {
                auto req = decode<get_dataset_bundles_request>(msg);
                if (!req) return;
                service::dataset_bundle_service svc(ctx);
                try {
                    const auto items = svc.list_bundles();
                    get_dataset_bundles_response resp;
                    resp.bundles = items;
                    resp.total_available_count = static_cast<int>(items.size());
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_dataset_bundles_response resp;
                    resp.total_available_count = 0;
                    reply(nats, msg, resp);
                }
                return;
            }

            if (msg.subject.ends_with("dataset-bundles.save")) {
                auto req = decode<save_dataset_bundle_request>(msg);
                if (!req) return;
                service::dataset_bundle_service svc(ctx);
                try {
                    svc.save_bundles(req->bundles);
                    reply(nats, msg, save_dataset_bundle_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        save_dataset_bundle_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("dataset-bundles.delete")) {
                auto req = decode<delete_dataset_bundle_request>(msg);
                if (!req) return;
                service::dataset_bundle_service svc(ctx);
                try {
                    boost::uuids::string_generator gen;
                    for (const auto& id : req->ids)
                        svc.remove_bundle(gen(id));
                    reply(nats, msg,
                        delete_dataset_bundle_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        delete_dataset_bundle_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("dataset-bundles.history")) {
                auto req =
                    decode<get_dataset_bundle_history_request>(msg);
                if (!req) return;
                service::dataset_bundle_service svc(ctx);
                try {
                    const auto history = svc.get_bundle_history(
                        boost::uuids::string_generator{}(req->id));
                    get_dataset_bundle_history_response resp;
                    resp.success = true;
                    resp.history = history;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_dataset_bundle_history_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }

            // ==================================================================
            // Publications
            // ==================================================================

            if (msg.subject.ends_with("publications.list")) {
                auto req = decode<get_publications_request>(msg);
                if (!req) return;
                service::publication_service svc(ctx);
                try {
                    boost::uuids::string_generator gen;
                    const auto pubs = svc.get_publication_history(
                        gen(req->dataset_id));
                    get_publications_response resp;
                    resp.success = true;
                    resp.publications = pubs;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_publications_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }

            // ==================================================================
            // Coding Scheme Authority Types
            // ==================================================================

            if (msg.subject.ends_with(
                    "coding-scheme-authority-types.list")) {
                auto req =
                    decode<get_coding_scheme_authority_types_request>(msg);
                if (!req) return;
                service::coding_scheme_service svc(ctx);
                try {
                    const auto items = svc.list_authority_types();
                    get_coding_scheme_authority_types_response resp;
                    resp.coding_scheme_authority_types = items;
                    resp.total_available_count = static_cast<int>(items.size());
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_coding_scheme_authority_types_response resp;
                    resp.total_available_count = 0;
                    reply(nats, msg, resp);
                }
                return;
            }

            if (msg.subject.ends_with(
                    "coding-scheme-authority-types.save")) {
                auto req =
                    decode<save_coding_scheme_authority_type_request>(msg);
                if (!req) return;
                service::coding_scheme_service svc(ctx);
                try {
                    svc.save_authority_type(req->data);
                    reply(nats, msg,
                        save_coding_scheme_authority_type_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        save_coding_scheme_authority_type_response{
                            false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with(
                    "coding-scheme-authority-types.delete")) {
                auto req =
                    decode<delete_coding_scheme_authority_type_request>(msg);
                if (!req) return;
                service::coding_scheme_service svc(ctx);
                try {
                    svc.remove_authority_types(req->types);
                    reply(nats, msg,
                        delete_coding_scheme_authority_type_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        delete_coding_scheme_authority_type_response{
                            false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with(
                    "coding-scheme-authority-types.history")) {
                auto req =
                    decode<get_coding_scheme_authority_type_history_request>(
                        msg);
                if (!req) return;
                service::coding_scheme_service svc(ctx);
                try {
                    const auto history =
                        svc.get_authority_type_history(req->type);
                    get_coding_scheme_authority_type_history_response resp;
                    resp.success = true;
                    resp.history = history;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_coding_scheme_authority_type_history_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }

            // ==================================================================
            // Coding Schemes
            // ==================================================================

            if (msg.subject.ends_with("coding-schemes.list")) {
                auto req = decode<get_coding_schemes_request>(msg);
                if (!req) return;
                service::coding_scheme_service svc(ctx);
                try {
                    const auto items = svc.list_coding_schemes(
                        static_cast<std::uint32_t>(req->offset),
                        static_cast<std::uint32_t>(req->limit));
                    const auto count = svc.get_coding_scheme_count();
                    get_coding_schemes_response resp;
                    resp.coding_schemes = items;
                    resp.total_available_count = static_cast<int>(count);
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_coding_schemes_response resp;
                    resp.total_available_count = 0;
                    reply(nats, msg, resp);
                }
                return;
            }

            if (msg.subject.ends_with("coding-schemes.save")) {
                auto req = decode<save_coding_scheme_request>(msg);
                if (!req) return;
                service::coding_scheme_service svc(ctx);
                try {
                    svc.save_coding_scheme(req->data);
                    reply(nats, msg, save_coding_scheme_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        save_coding_scheme_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("coding-schemes.delete")) {
                auto req = decode<delete_coding_scheme_request>(msg);
                if (!req) return;
                service::coding_scheme_service svc(ctx);
                try {
                    svc.remove_coding_schemes(req->codes);
                    reply(nats, msg, delete_coding_scheme_response{true, {}});
                } catch (const std::exception& e) {
                    reply(nats, msg,
                        delete_coding_scheme_response{false, e.what()});
                }
                return;
            }

            if (msg.subject.ends_with("coding-schemes.history")) {
                auto req = decode<get_coding_scheme_history_request>(msg);
                if (!req) return;
                service::coding_scheme_service svc(ctx);
                try {
                    const auto history =
                        svc.get_coding_scheme_history(req->code);
                    get_coding_scheme_history_response resp;
                    resp.success = true;
                    resp.history = history;
                    reply(nats, msg, resp);
                } catch (const std::exception& e) {
                    get_coding_scheme_history_response resp;
                    resp.success = false;
                    resp.message = e.what();
                    reply(nats, msg, resp);
                }
                return;
            }
        }));

    return subs;
}

}
