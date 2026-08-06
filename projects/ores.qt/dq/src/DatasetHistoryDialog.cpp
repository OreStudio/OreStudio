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
#include "ores.qt/DatasetHistoryDialog.hpp"
#include "ores.dq.api/messaging/dataset_protocol.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ui_DatasetHistoryDialog.h"
#include <QHeaderView>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

DatasetHistoryDialog::DatasetHistoryDialog(const boost::uuids::uuid& id,
                                           ClientManager* clientManager,
                                           QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::DatasetHistoryDialog)
    , id_(id)
    , clientManager_(clientManager) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    ui_->titleLabel->setText(QString("Dataset History"));

    ui_->versionListWidget->setColumnCount(4);
    ui_->versionListWidget->setHorizontalHeaderLabels(
        {"Version", "Recorded At", "Modified By", "Commentary"});

    initializeHistoryUi({.versionList = ui_->versionListWidget,
                         .changesTable = ui_->changesTableWidget,
                         .titleLabel = ui_->titleLabel,
                         .closeButton = ui_->closeButton});
}

DatasetHistoryDialog::~DatasetHistoryDialog() = default;

QString DatasetHistoryDialog::code() const {
    return QString::fromStdString(boost::uuids::to_string(id_));
}

void DatasetHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for dataset: " << boost::uuids::to_string(id_);
    emit statusChanged(tr("Loading history..."));

    dq::messaging::get_dataset_history_request request;
    request.id = boost::uuids::to_string(id_);

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        versions_ = std::move(response.history);
        historyLoaded();
    });
}

int DatasetHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow DatasetHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.version,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.change_commentary)}};
}

QString DatasetHistoryDialog::historyTitle() const {
    return QString("Dataset History");
}

HistoryDialogBase::DiffResult DatasetHistoryDialog::calculateDiffAt(int current_index,
                                                                    int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Name", current.name, previous.name);
    checkString(diffs, "Catalog", current.catalog_name, previous.catalog_name);
    checkString(diffs, "Subject Area", current.subject_area_name, previous.subject_area_name);
    checkString(diffs, "Domain", current.domain_name, previous.domain_name);
    checkString(diffs, "Source System", current.source_system_id, previous.source_system_id);
    checkString(diffs, "Description", current.description, previous.description);

    return diffs;
}

void DatasetHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->nameValue->setText(QString::fromStdString(version.name));
    ui_->catalogValue->setText(QString::fromStdString(version.catalog_name.value_or("")));
    ui_->subjectAreaValue->setText(QString::fromStdString(version.subject_area_name));
    ui_->domainValue->setText(QString::fromStdString(version.domain_name));
    ui_->sourceSystemValue->setText(QString::fromStdString(version.source_system_id));
    ui_->descriptionValue->setText(QString::fromStdString(version.description));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void DatasetHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening dataset version " << version.version
                              << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void DatasetHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; revert TO the
    // selected version. The server handles versioning.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version " << selected.version;

    emit revertVersionRequested(selected);
}

}
