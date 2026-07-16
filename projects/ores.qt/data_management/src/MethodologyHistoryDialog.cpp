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
#include "ores.qt/MethodologyHistoryDialog.hpp"
#include "ores.dq.api/messaging/data_organization_protocol.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ui_MethodologyHistoryDialog.h"
#include <QHeaderView>

namespace ores::qt {

using namespace ores::logging;

namespace {

QString optionalText(const std::optional<std::string>& value) {
    return QString::fromStdString(value.value_or(""));
}

}

MethodologyHistoryDialog::MethodologyHistoryDialog(const std::string& name,
                                                   ClientManager* clientManager,
                                                   QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::MethodologyHistoryDialog)
    , name_(name)
    , clientManager_(clientManager) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    ui_->titleLabel->setText(QString("Methodology History"));

    ui_->versionListWidget->setColumnCount(4);
    ui_->versionListWidget->setHorizontalHeaderLabels(
        {"Version", "Recorded At", "Modified By", "Commentary"});

    initializeHistoryUi({.versionList = ui_->versionListWidget,
                         .changesTable = ui_->changesTableWidget,
                         .titleLabel = ui_->titleLabel,
                         .closeButton = ui_->closeButton});
}

MethodologyHistoryDialog::~MethodologyHistoryDialog() = default;

void MethodologyHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for methodology: " << name_;
    emit statusChanged(tr("Loading history..."));

    dq::messaging::get_methodology_history_request request;
    request.code = name_;

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

int MethodologyHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow MethodologyHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.version,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.change_commentary)}};
}

QString MethodologyHistoryDialog::historyTitle() const {
    return QString("Methodology History");
}

HistoryDialogBase::DiffResult MethodologyHistoryDialog::calculateDiffAt(int current_index,
                                                                        int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Name", current.name, previous.name);
    checkString(diffs, "Description", current.description, previous.description);
    checkString(diffs, "Logic Reference", current.logic_reference, previous.logic_reference);
    checkString(
        diffs, "Implementation", current.implementation_details, previous.implementation_details);

    return diffs;
}

void MethodologyHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->nameValue->setText(QString::fromStdString(version.name));
    ui_->descriptionValue->setText(QString::fromStdString(version.description));
    ui_->logicReferenceValue->setText(optionalText(version.logic_reference));
    ui_->implementationValue->setText(optionalText(version.implementation_details));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void MethodologyHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening methodology version " << version.version
                              << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void MethodologyHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; the server handles
    // versioning.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version " << selected.version;

    emit revertVersionRequested(selected);
}

}
