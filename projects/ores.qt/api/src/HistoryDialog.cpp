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
#include "ores.qt/HistoryDialog.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.diff/engine/compare.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ui_HistoryDialog.h"
#include <algorithm>
#include <iterator>
#include <set>

namespace ores::qt {

using namespace ores::logging;
using ores::history::messaging::entity_history_version;
using ores::history::messaging::get_entity_history_request;

HistoryDialog::HistoryDialog(std::string entityType,
                             std::string entityId,
                             ClientManager* clientManager,
                             QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::HistoryDialog)
    , entityType_(std::move(entityType))
    , entityId_(std::move(entityId))
    , clientManager_(clientManager) {

    ui_->setupUi(this);
    initializeHistoryUi({.versionList = nullptr,
                         .versionTimeline = ui_->versionTimeline,
                         .changesTable = nullptr,
                         .diffBrowser = ui_->diffBrowser,
                         .titleLabel = nullptr,
                         .closeButton = ui_->closeButton,
                         .compareFromCombo = ui_->compareFromCombo,
                         .compareToCombo = ui_->compareToCombo,
                         .allFieldsToggle = ui_->allFieldsToggle,
                         .onlyChangesToggle = ui_->onlyChangesToggle});
}

HistoryDialog::~HistoryDialog() {
    delete ui_;
}

QString HistoryDialog::code() const {
    return QString::fromStdString(entityId_);
}

void HistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for " << entityType_ << " " << entityId_;
    emit statusChanged(tr("Loading history..."));

    get_entity_history_request request;
    request.entity_type = entityType_;
    request.entity_id = entityId_;

    QPointer<HistoryDialog> self = this;
    runHistoryRequest(clientManager_, std::move(request), [self](auto response) {
        if (!self)
            return;
        if (!response.success) {
            self->historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        self->versions_ = std::move(response.versions);
        self->historyLoaded();
    });
}

int HistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

QString HistoryDialog::historyTitle() const {
    return tr("History for: %1").arg(QString::fromStdString(entityId_));
}

namespace {

// Field names shown as provenance in the timeline card rather than as
// diffable rows — they describe who/why a version was recorded, not
// what changed in it, so diffing them is either meaningless (Recorded
// At differs on every version by construction) or redundant (already
// shown, unabbreviated, on the card itself).
bool isTimelineProvenanceField(const std::string& name) {
    using ores::history::domain::provenance_fields;
    static const std::set<std::string> fields{provenance_fields::recorded_at,
                                              provenance_fields::modified_by,
                                              provenance_fields::performed_by,
                                              provenance_fields::change_reason_code,
                                              provenance_fields::change_commentary};
    return fields.contains(name);
}

QString fieldValue(const std::vector<ores::diff::domain::field_value>& fields,
                   const std::string& name) {
    const auto it =
        std::find_if(fields.begin(), fields.end(), [&](const auto& f) { return f.name == name; });
    return it != fields.end() ? QString::fromStdString(it->value) : QString();
}

}

HistoryDialogBase::VersionRow HistoryDialog::versionRow(int index) const {
    const auto& v = versions_[index];
    const auto& fields = v.fields;
    using ores::history::domain::provenance_fields;
    return {
        v.version,
        {relative_time_helper::format(v.recorded_at),
         QString::fromStdString(v.modified_by),
         fieldValue(fields, provenance_fields::performed_by),
         fieldValue(fields, provenance_fields::change_reason_code),
         fieldValue(fields, provenance_fields::change_commentary),
         QString::fromStdString(ores::platform::time::datetime::to_iso8601_utc(v.recorded_at))}};
}

HistoryDialogBase::DiffResult
HistoryDialog::calculateDiffBetween(int index_new, int index_old, bool include_unchanged) const {
    auto without_provenance_fields = [](const auto& fields) {
        std::vector<ores::diff::domain::field_value> filtered;
        filtered.reserve(fields.size());
        std::copy_if(fields.begin(), fields.end(), std::back_inserter(filtered), [](const auto& f) {
            return !isTimelineProvenanceField(f.name);
        });
        return filtered;
    };
    const auto old_fields = without_provenance_fields(versions_[index_old].fields);
    const auto new_fields = without_provenance_fields(versions_[index_new].fields);
    const auto changes = ores::diff::engine::compute(old_fields, new_fields);

    DiffResult diffs;
    if (!include_unchanged) {
        for (const auto& e : changes.entries) {
            diffs.append(
                {QString::fromStdString(e.field_name),
                 {QString::fromStdString(e.old_value), QString::fromStdString(e.new_value)}});
        }
        return diffs;
    }

    // All Fields: every current-version field, changed or not, in its
    // own order; fields removed since the older version are appended
    // last, matching the diff_result contract's ordering.
    for (const auto& f : new_fields) {
        const auto it = std::find_if(changes.entries.begin(),
                                     changes.entries.end(),
                                     [&](const auto& e) { return e.field_name == f.name; });
        if (it != changes.entries.end()) {
            diffs.append(
                {QString::fromStdString(f.name),
                 {QString::fromStdString(it->old_value), QString::fromStdString(it->new_value)}});
        } else {
            diffs.append({QString::fromStdString(f.name),
                          {QString::fromStdString(f.value), QString::fromStdString(f.value)}});
        }
    }
    for (const auto& e : changes.entries) {
        const auto in_new = std::any_of(new_fields.begin(), new_fields.end(), [&](const auto& f) {
            return f.name == e.field_name;
        });
        if (!in_new)
            diffs.append(
                {QString::fromStdString(e.field_name),
                 {QString::fromStdString(e.old_value), QString::fromStdString(e.new_value)}});
    }
    return diffs;
}

void HistoryDialog::openVersionAt(int index) {
    emit openVersionRequested(QString::fromStdString(entityType_),
                              QString::fromStdString(entityId_),
                              versions_[index].version);
}

void HistoryDialog::revertToVersionAt(int index) {
    emit revertVersionRequested(QString::fromStdString(entityType_),
                                QString::fromStdString(entityId_),
                                versions_[index].version);
}

}
