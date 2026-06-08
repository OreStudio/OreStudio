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
#include "ores.qt/PricingModelProductParameterHistoryDialog.hpp"
#include "ores.analytics.api/messaging/pricing_model_product_parameter_protocol.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ui_PricingModelProductParameterHistoryDialog.h"
#include <QHeaderView>

namespace ores::qt {

using namespace ores::logging;

PricingModelProductParameterHistoryDialog::PricingModelProductParameterHistoryDialog(
    const boost::uuids::uuid& id,
    const QString& code,
    ClientManager* clientManager,
    QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::PricingModelProductParameterHistoryDialog)
    , id_(id)
    , code_(code)
    , clientManager_(clientManager) {

    ui_->setupUi(this);

    ui_->titleLabel->setText(QString("History for: %1").arg(code_));

    ui_->versionListWidget->setColumnCount(5);
    ui_->versionListWidget->setHorizontalHeaderLabels(
        {"Version", "Recorded At", "Modified By", "Performed By", "Commentary"});

    initializeHistoryUi({.versionList = ui_->versionListWidget,
                         .changesTable = ui_->changesTableWidget,
                         .titleLabel = ui_->titleLabel,
                         .closeButton = ui_->closeButton});
}

PricingModelProductParameterHistoryDialog::~PricingModelProductParameterHistoryDialog() = default;

void PricingModelProductParameterHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for pricing model product parameter: "
                               << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    analytics::messaging::get_pricing_model_product_parameter_history_request request;
    request.id = id_;

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        versions_ = std::move(response.parameters);
        historyLoaded();
    });
}

int PricingModelProductParameterHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow
PricingModelProductParameterHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.version,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.performed_by),
                      QString::fromStdString(version.change_commentary)}};
}

QString PricingModelProductParameterHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::DiffResult
PricingModelProductParameterHistoryDialog::calculateDiffAt(int current_index,
                                                           int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Scope", current.parameter_scope, previous.parameter_scope);
    checkString(diffs, "Name", current.parameter_name, previous.parameter_name);
    checkString(diffs, "Value", current.parameter_value, previous.parameter_value);

    return diffs;
}

void PricingModelProductParameterHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->parameterScopeValue->setText(QString::fromStdString(version.parameter_scope));
    ui_->parameterNameValue->setText(QString::fromStdString(version.parameter_name));
    ui_->parameterValueValue->setText(QString::fromStdString(version.parameter_value));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void PricingModelProductParameterHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening pricing model product parameter version "
                              << version.version << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void PricingModelProductParameterHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; revert TO the
    // selected version. The server handles versioning.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version " << selected.version;

    emit revertVersionRequested(selected);
}

}
