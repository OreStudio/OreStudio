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
#include "ores.dq.core/messaging/registrar.hpp"
#include "ores.dq.api/messaging/catalog_protocol.hpp"
#include "ores.dq.api/messaging/change_reason_category_protocol.hpp"
#include "ores.dq.api/messaging/change_reason_protocol.hpp"
#include "ores.dq.api/messaging/coding_scheme_protocol.hpp"
#include "ores.dq.api/messaging/data_organization_protocol.hpp"
#include "ores.dq.api/messaging/dataset_bundle_member_protocol.hpp"
#include "ores.dq.api/messaging/dataset_bundle_protocol.hpp"
#include "ores.dq.api/messaging/dataset_dependency_protocol.hpp"
#include "ores.dq.api/messaging/dataset_protocol.hpp"
#include "ores.dq.api/messaging/fsm_protocol.hpp"
#include "ores.dq.api/messaging/lei_entity_summary_protocol.hpp"
#include "ores.dq.api/messaging/publication_protocol.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.dq.api/messaging/report_definition_template_protocol.hpp"
#include "ores.dq.core/messaging/badge_definition_registrar.hpp"
#include "ores.dq.core/messaging/badge_handler.hpp"
#include "ores.dq.core/messaging/badge_severity_registrar.hpp"
#include "ores.dq.core/messaging/catalog_registrar.hpp"
#include "ores.dq.core/messaging/change_reason_category_registrar.hpp"
#include "ores.dq.core/messaging/change_reason_registrar.hpp"
#include "ores.dq.core/messaging/code_domain_registrar.hpp"
#include "ores.dq.core/messaging/coding_scheme_handler.hpp"
#include "ores.dq.core/messaging/data_organization_handler.hpp"
#include "ores.dq.core/messaging/dataset_bundle_handler.hpp"
#include "ores.dq.core/messaging/dataset_bundle_member_handler.hpp"
#include "ores.dq.core/messaging/dataset_dependency_handler.hpp"
#include "ores.dq.core/messaging/dataset_handler.hpp"
#include "ores.dq.core/messaging/dimension_handler.hpp"
#include "ores.dq.core/messaging/fsm_handler.hpp"
#include "ores.dq.core/messaging/lei_entity_handler.hpp"
#include "ores.dq.core/messaging/publication_handler.hpp"
#include "ores.dq.core/messaging/publish_from_dq_handler.hpp"
#include "ores.dq.core/messaging/report_definition_template_handler.hpp"
#include <memory>

namespace ores::dq::messaging {

namespace {

constexpr std::string_view queue_group = "ores.dq.service";

}

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // =========================================================================
    // FSM States
    // =========================================================================

    auto fsm = std::make_shared<fsm_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_fsm_states_request::nats_subject, queue_group, [fsm](ores::nats::message msg) {
            fsm->list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        get_fsm_transitions_request::nats_subject, queue_group, [fsm](ores::nats::message msg) {
            fsm->list_transitions(std::move(msg));
        }));

    // =========================================================================
    // Change reason category and change reason are both on the standard
    // generated stack (see change_reason_category_handler/_registrar and
    // change_reason_handler/_registrar).
    // =========================================================================

    {
        auto change_reason_category_subs =
            register_change_reason_category_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                   std::make_move_iterator(change_reason_category_subs.begin()),
                   std::make_move_iterator(change_reason_category_subs.end()));

        auto change_reason_subs = register_change_reason_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                   std::make_move_iterator(change_reason_subs.begin()),
                   std::make_move_iterator(change_reason_subs.end()));
    }

    // =========================================================================
    // Catalog is on the standard generated stack (see catalog_handler/
    // catalog_registrar); data-domains, methodologies, and subject-areas
    // stay on the bespoke data_organization_handler for now.
    // =========================================================================

    {
        auto catalog_subs = register_catalog_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                   std::make_move_iterator(catalog_subs.begin()),
                   std::make_move_iterator(catalog_subs.end()));
    }

    // =========================================================================
    // Data Organization (data-domains, methodologies, subject-areas)
    // =========================================================================

    auto do_ = std::make_shared<data_organization_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_data_domains_request::nats_subject, queue_group, [do_](ores::nats::message msg) {
            do_->list_data_domains(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        save_data_domain_request::nats_subject, queue_group, [do_](ores::nats::message msg) {
            do_->save_data_domain(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        delete_data_domain_request::nats_subject, queue_group, [do_](ores::nats::message msg) {
            do_->delete_data_domains(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        get_data_domain_history_request::nats_subject, queue_group, [do_](ores::nats::message msg) {
            do_->data_domain_history(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        get_methodologies_request::nats_subject, queue_group, [do_](ores::nats::message msg) {
            do_->list_methodologies(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        save_methodology_request::nats_subject, queue_group, [do_](ores::nats::message msg) {
            do_->save_methodology(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        delete_methodology_request::nats_subject, queue_group, [do_](ores::nats::message msg) {
            do_->delete_methodologies(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        get_methodology_history_request::nats_subject, queue_group, [do_](ores::nats::message msg) {
            do_->methodology_history(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        get_subject_areas_request::nats_subject, queue_group, [do_](ores::nats::message msg) {
            do_->list_subject_areas(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        save_subject_area_request::nats_subject, queue_group, [do_](ores::nats::message msg) {
            do_->save_subject_area(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        delete_subject_area_request::nats_subject, queue_group, [do_](ores::nats::message msg) {
            do_->delete_subject_areas(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        get_subject_area_history_request::nats_subject,
        queue_group,
        [do_](ores::nats::message msg) { do_->subject_area_history(std::move(msg)); }));

    // =========================================================================
    // Dimensions (nature, origin, treatment)
    // =========================================================================

    auto dim = std::make_shared<dimension_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_nature_dimensions_request::nats_subject, queue_group, [dim](ores::nats::message msg) {
            dim->list_nature_dimensions(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        save_nature_dimension_request::nats_subject, queue_group, [dim](ores::nats::message msg) {
            dim->save_nature_dimension(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        delete_nature_dimension_request::nats_subject, queue_group, [dim](ores::nats::message msg) {
            dim->delete_nature_dimensions(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        get_nature_dimension_history_request::nats_subject,
        queue_group,
        [dim](ores::nats::message msg) { dim->nature_dimension_history(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_origin_dimensions_request::nats_subject, queue_group, [dim](ores::nats::message msg) {
            dim->list_origin_dimensions(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        save_origin_dimension_request::nats_subject, queue_group, [dim](ores::nats::message msg) {
            dim->save_origin_dimension(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        delete_origin_dimension_request::nats_subject, queue_group, [dim](ores::nats::message msg) {
            dim->delete_origin_dimensions(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        get_origin_dimension_history_request::nats_subject,
        queue_group,
        [dim](ores::nats::message msg) { dim->origin_dimension_history(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_treatment_dimensions_request::nats_subject,
        queue_group,
        [dim](ores::nats::message msg) { dim->list_treatment_dimensions(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_treatment_dimension_request::nats_subject,
        queue_group,
        [dim](ores::nats::message msg) { dim->save_treatment_dimension(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_treatment_dimension_request::nats_subject,
        queue_group,
        [dim](ores::nats::message msg) { dim->delete_treatment_dimensions(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_treatment_dimension_history_request::nats_subject,
        queue_group,
        [dim](ores::nats::message msg) { dim->treatment_dimension_history(std::move(msg)); }));

    // =========================================================================
    // Datasets
    // =========================================================================

    auto ds = std::make_shared<dataset_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_datasets_request::nats_subject, queue_group, [ds](ores::nats::message msg) {
            ds->list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        save_dataset_request::nats_subject, queue_group, [ds](ores::nats::message msg) {
            ds->save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        delete_dataset_request::nats_subject, queue_group, [ds](ores::nats::message msg) {
            ds->remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        get_dataset_history_request::nats_subject, queue_group, [ds](ores::nats::message msg) {
            ds->history(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        publish_datasets_request::nats_subject, queue_group, [ds](ores::nats::message msg) {
            ds->publish(std::move(msg));
        }));

    // =========================================================================
    // Dataset Bundles
    // =========================================================================

    auto db = std::make_shared<dataset_bundle_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_dataset_bundles_request::nats_subject, queue_group, [db](ores::nats::message msg) {
            db->list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        save_dataset_bundle_request::nats_subject, queue_group, [db](ores::nats::message msg) {
            db->save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        delete_dataset_bundle_request::nats_subject, queue_group, [db](ores::nats::message msg) {
            db->remove(std::move(msg));
        }));

    subs.push_back(
        nats.queue_subscribe(get_dataset_bundle_history_request::nats_subject,
                             queue_group,
                             [db](ores::nats::message msg) { db->history(std::move(msg)); }));

    // =========================================================================
    // Dataset Bundle Members
    // =========================================================================

    auto dbm = std::make_shared<dataset_bundle_member_handler>(nats, ctx, verifier);

    subs.push_back(
        nats.queue_subscribe(get_dataset_bundle_members_request::nats_subject,
                             queue_group,
                             [dbm](ores::nats::message msg) { dbm->list(std::move(msg)); }));

    subs.push_back(
        nats.queue_subscribe(get_dataset_bundle_members_by_bundle_request::nats_subject,
                             queue_group,
                             [dbm](ores::nats::message msg) { dbm->by_bundle(std::move(msg)); }));

    // =========================================================================
    // Dataset Dependencies
    // =========================================================================

    auto dep = std::make_shared<dataset_dependency_handler>(nats, ctx, verifier);

    subs.push_back(
        nats.queue_subscribe(get_dataset_dependencies_request::nats_subject,
                             queue_group,
                             [dep](ores::nats::message msg) { dep->list(std::move(msg)); }));

    subs.push_back(
        nats.queue_subscribe(get_dataset_dependencies_by_dataset_request::nats_subject,
                             queue_group,
                             [dep](ores::nats::message msg) { dep->by_dataset(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        resolve_dependencies_request::nats_subject, queue_group, [dep](ores::nats::message msg) {
            dep->resolve(std::move(msg));
        }));

    // =========================================================================
    // Publications
    // =========================================================================

    auto pub = std::make_shared<publication_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_publications_request::nats_subject, queue_group, [pub](ores::nats::message msg) {
            pub->list_publications(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        publish_bundle_request::nats_subject, queue_group, [pub](ores::nats::message msg) {
            pub->publish_bundle(std::move(msg));
        }));

    // =========================================================================
    // Coding Schemes
    // =========================================================================

    auto cs = std::make_shared<coding_scheme_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_coding_scheme_authority_types_request::nats_subject,
        queue_group,
        [cs](ores::nats::message msg) { cs->list_authority_types(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_coding_scheme_authority_type_request::nats_subject,
        queue_group,
        [cs](ores::nats::message msg) { cs->save_authority_type(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_coding_scheme_authority_type_request::nats_subject,
        queue_group,
        [cs](ores::nats::message msg) { cs->delete_authority_types(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_coding_scheme_authority_type_history_request::nats_subject,
        queue_group,
        [cs](ores::nats::message msg) { cs->authority_type_history(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_coding_schemes_request::nats_subject, queue_group, [cs](ores::nats::message msg) {
            cs->list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        save_coding_scheme_request::nats_subject, queue_group, [cs](ores::nats::message msg) {
            cs->save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        delete_coding_scheme_request::nats_subject, queue_group, [cs](ores::nats::message msg) {
            cs->remove(std::move(msg));
        }));

    subs.push_back(
        nats.queue_subscribe(get_coding_scheme_history_request::nats_subject,
                             queue_group,
                             [cs](ores::nats::message msg) { cs->history(std::move(msg)); }));

    // =========================================================================
    // LEI Entities
    // =========================================================================

    auto lei = std::make_shared<lei_entity_handler>(nats, ctx, verifier);

    subs.push_back(
        nats.queue_subscribe(get_lei_entities_summary_request::nats_subject,
                             queue_group,
                             [lei](ores::nats::message msg) { lei->summary(std::move(msg)); }));

    // =========================================================================
    // Report Definition Templates (served from DQ artefact tables)
    // =========================================================================

    {
        auto rdt = std::make_shared<report_definition_template_handler>(nats, ctx, verifier);
        subs.push_back(
            nats.queue_subscribe(list_dq_report_definition_templates_request::nats_subject,
                                 queue_group,
                                 [rdt](ores::nats::message msg) { rdt->list(std::move(msg)); }));
    }

    // =========================================================================
    // Badges: severities, code domains, and definitions are on the standard
    // generated stack (see badge_definition_handler/badge_severity_handler/
    // code_domain_handler); badge_mapping is a junction with no generated
    // handler of its own, so it stays on the bespoke badge_handler.
    // =========================================================================

    {
        auto badge_definition_subs = register_badge_definition_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(badge_definition_subs.begin()),
                    std::make_move_iterator(badge_definition_subs.end()));

        auto badge_severity_subs = register_badge_severity_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(badge_severity_subs.begin()),
                    std::make_move_iterator(badge_severity_subs.end()));

        auto code_domain_subs = register_code_domain_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(code_domain_subs.begin()),
                    std::make_move_iterator(code_domain_subs.end()));
    }

    auto badge = std::make_shared<badge_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_badge_mappings_request::nats_subject, queue_group, [badge](ores::nats::message msg) {
            badge->list_mappings(std::move(msg));
        }));

    // =========================================================================
    // DQ-internal Publish-from-DQ workflow step handlers
    // =========================================================================

    {
        auto pdq = std::make_shared<publish_from_dq_handler>(nats, ctx);
        subs.push_back(nats.queue_subscribe(
            "dq.v1.ip2country.publish-from-dq", queue_group, [pdq](ores::nats::message msg) {
                pdq->handle(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            "dq.v1.coding-schemes.publish-from-dq", queue_group, [pdq](ores::nats::message msg) {
                pdq->handle(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            "dq.v1.badge-severities.publish-from-dq", queue_group, [pdq](ores::nats::message msg) {
                pdq->handle(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            "dq.v1.badge-definitions.publish-from-dq", queue_group, [pdq](ores::nats::message msg) {
                pdq->handle(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            "dq.v1.code-domains.publish-from-dq", queue_group, [pdq](ores::nats::message msg) {
                pdq->handle(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            "dq.v1.badge-mappings.publish-from-dq", queue_group, [pdq](ores::nats::message msg) {
                pdq->handle(std::move(msg));
            }));
    }

    return subs;
}

}
