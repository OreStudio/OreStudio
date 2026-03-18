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
#include <memory>
#include "ores.dq/messaging/registrar.hpp"
#include "ores.dq/messaging/change_management_protocol.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.dq/messaging/dataset_protocol.hpp"
#include "ores.dq/messaging/dataset_bundle_protocol.hpp"
#include "ores.dq/messaging/dataset_bundle_member_protocol.hpp"
#include "ores.dq/messaging/dataset_dependency_protocol.hpp"
#include "ores.dq/messaging/publication_protocol.hpp"
#include "ores.dq/messaging/publish_bundle_protocol.hpp"
#include "ores.dq/messaging/coding_scheme_protocol.hpp"
#include "ores.dq/messaging/lei_entity_summary_protocol.hpp"
#include "ores.dq/messaging/change_management_handler.hpp"
#include "ores.dq/messaging/data_organization_handler.hpp"
#include "ores.dq/messaging/dimension_handler.hpp"
#include "ores.dq/messaging/dataset_handler.hpp"
#include "ores.dq/messaging/dataset_bundle_handler.hpp"
#include "ores.dq/messaging/dataset_bundle_member_handler.hpp"
#include "ores.dq/messaging/dataset_dependency_handler.hpp"
#include "ores.dq/messaging/publication_handler.hpp"
#include "ores.dq/messaging/coding_scheme_handler.hpp"
#include "ores.dq/messaging/lei_entity_handler.hpp"

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
    // Change Management
    // =========================================================================

    auto cm = std::make_shared<change_management_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_change_reason_categories_request::nats_subject, queue_group,
        [cm](ores::nats::message msg) {
            cm->list_categories(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_change_reason_category_request::nats_subject, queue_group,
        [cm](ores::nats::message msg) {
            cm->save_category(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_change_reason_category_request::nats_subject, queue_group,
        [cm](ores::nats::message msg) {
            cm->delete_categories(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_change_reason_category_history_request::nats_subject, queue_group,
        [cm](ores::nats::message msg) {
            cm->category_history(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_change_reasons_request::nats_subject, queue_group,
        [cm](ores::nats::message msg) {
            cm->list_reasons(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_change_reason_request::nats_subject, queue_group,
        [cm](ores::nats::message msg) {
            cm->save_reason(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_change_reason_request::nats_subject, queue_group,
        [cm](ores::nats::message msg) {
            cm->delete_reasons(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_change_reason_history_request::nats_subject, queue_group,
        [cm](ores::nats::message msg) {
            cm->reason_history(std::move(msg)); }));

    // =========================================================================
    // Data Organization (catalogs, data-domains, methodologies, subject-areas)
    // =========================================================================

    auto do_ = std::make_shared<data_organization_handler>(
        nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_catalogs_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->list_catalogs(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_catalog_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->save_catalog(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_catalog_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->delete_catalogs(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_catalog_history_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->catalog_history(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_data_domains_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->list_data_domains(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_data_domain_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->save_data_domain(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_data_domain_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->delete_data_domains(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_data_domain_history_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->data_domain_history(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_methodologies_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->list_methodologies(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_methodology_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->save_methodology(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_methodology_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->delete_methodologies(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_methodology_history_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->methodology_history(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_subject_areas_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->list_subject_areas(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_subject_area_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->save_subject_area(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_subject_area_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->delete_subject_areas(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_subject_area_history_request::nats_subject, queue_group,
        [do_](ores::nats::message msg) {
            do_->subject_area_history(std::move(msg)); }));

    // =========================================================================
    // Dimensions (nature, origin, treatment)
    // =========================================================================

    auto dim = std::make_shared<dimension_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_nature_dimensions_request::nats_subject, queue_group,
        [dim](ores::nats::message msg) {
            dim->list_nature_dimensions(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_nature_dimension_request::nats_subject, queue_group,
        [dim](ores::nats::message msg) {
            dim->save_nature_dimension(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_nature_dimension_request::nats_subject, queue_group,
        [dim](ores::nats::message msg) {
            dim->delete_nature_dimensions(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_nature_dimension_history_request::nats_subject, queue_group,
        [dim](ores::nats::message msg) {
            dim->nature_dimension_history(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_origin_dimensions_request::nats_subject, queue_group,
        [dim](ores::nats::message msg) {
            dim->list_origin_dimensions(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_origin_dimension_request::nats_subject, queue_group,
        [dim](ores::nats::message msg) {
            dim->save_origin_dimension(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_origin_dimension_request::nats_subject, queue_group,
        [dim](ores::nats::message msg) {
            dim->delete_origin_dimensions(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_origin_dimension_history_request::nats_subject, queue_group,
        [dim](ores::nats::message msg) {
            dim->origin_dimension_history(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_treatment_dimensions_request::nats_subject, queue_group,
        [dim](ores::nats::message msg) {
            dim->list_treatment_dimensions(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_treatment_dimension_request::nats_subject, queue_group,
        [dim](ores::nats::message msg) {
            dim->save_treatment_dimension(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_treatment_dimension_request::nats_subject, queue_group,
        [dim](ores::nats::message msg) {
            dim->delete_treatment_dimensions(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_treatment_dimension_history_request::nats_subject, queue_group,
        [dim](ores::nats::message msg) {
            dim->treatment_dimension_history(std::move(msg)); }));

    // =========================================================================
    // Datasets
    // =========================================================================

    auto ds = std::make_shared<dataset_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_datasets_request::nats_subject, queue_group,
        [ds](ores::nats::message msg) { ds->list(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_dataset_request::nats_subject, queue_group,
        [ds](ores::nats::message msg) { ds->save(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_dataset_request::nats_subject, queue_group,
        [ds](ores::nats::message msg) { ds->remove(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_dataset_history_request::nats_subject, queue_group,
        [ds](ores::nats::message msg) { ds->history(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        publish_datasets_request::nats_subject, queue_group,
        [ds](ores::nats::message msg) { ds->publish(std::move(msg)); }));

    // =========================================================================
    // Dataset Bundles
    // =========================================================================

    auto db = std::make_shared<dataset_bundle_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_dataset_bundles_request::nats_subject, queue_group,
        [db](ores::nats::message msg) { db->list(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_dataset_bundle_request::nats_subject, queue_group,
        [db](ores::nats::message msg) { db->save(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_dataset_bundle_request::nats_subject, queue_group,
        [db](ores::nats::message msg) { db->remove(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_dataset_bundle_history_request::nats_subject, queue_group,
        [db](ores::nats::message msg) { db->history(std::move(msg)); }));

    // =========================================================================
    // Dataset Bundle Members
    // =========================================================================

    auto dbm = std::make_shared<dataset_bundle_member_handler>(
        nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_dataset_bundle_members_request::nats_subject, queue_group,
        [dbm](ores::nats::message msg) { dbm->list(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_dataset_bundle_members_by_bundle_request::nats_subject, queue_group,
        [dbm](ores::nats::message msg) {
            dbm->by_bundle(std::move(msg)); }));

    // =========================================================================
    // Dataset Dependencies
    // =========================================================================

    auto dep = std::make_shared<dataset_dependency_handler>(
        nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_dataset_dependencies_request::nats_subject, queue_group,
        [dep](ores::nats::message msg) { dep->list(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_dataset_dependencies_by_dataset_request::nats_subject, queue_group,
        [dep](ores::nats::message msg) {
            dep->by_dataset(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        resolve_dependencies_request::nats_subject, queue_group,
        [dep](ores::nats::message msg) { dep->resolve(std::move(msg)); }));

    // =========================================================================
    // Publications
    // =========================================================================

    auto pub = std::make_shared<publication_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_publications_request::nats_subject, queue_group,
        [pub](ores::nats::message msg) {
            pub->list_publications(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        publish_bundle_request::nats_subject, queue_group,
        [pub](ores::nats::message msg) {
            pub->publish_bundle(std::move(msg)); }));

    // =========================================================================
    // Coding Schemes
    // =========================================================================

    auto cs = std::make_shared<coding_scheme_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_coding_scheme_authority_types_request::nats_subject, queue_group,
        [cs](ores::nats::message msg) {
            cs->list_authority_types(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_coding_scheme_authority_type_request::nats_subject, queue_group,
        [cs](ores::nats::message msg) {
            cs->save_authority_type(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_coding_scheme_authority_type_request::nats_subject, queue_group,
        [cs](ores::nats::message msg) {
            cs->delete_authority_types(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_coding_scheme_authority_type_history_request::nats_subject,
        queue_group,
        [cs](ores::nats::message msg) {
            cs->authority_type_history(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_coding_schemes_request::nats_subject, queue_group,
        [cs](ores::nats::message msg) { cs->list(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_coding_scheme_request::nats_subject, queue_group,
        [cs](ores::nats::message msg) { cs->save(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        delete_coding_scheme_request::nats_subject, queue_group,
        [cs](ores::nats::message msg) { cs->remove(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        get_coding_scheme_history_request::nats_subject, queue_group,
        [cs](ores::nats::message msg) { cs->history(std::move(msg)); }));

    // =========================================================================
    // LEI Entities
    // =========================================================================

    auto lei = std::make_shared<lei_entity_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        get_lei_entities_summary_request::nats_subject, queue_group,
        [lei](ores::nats::message msg) { lei->summary(std::move(msg)); }));

    return subs;
}

}
