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
#include "ores.qt/TradeHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ui_TradeHistoryDialog.h"
#include <QHeaderView>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

namespace {

QString optionalText(const std::optional<std::string>& value) {
    return value ? QString::fromStdString(*value) : QString{};
}

}

TradeHistoryDialog::TradeHistoryDialog(const boost::uuids::uuid& id,
                                       const QString& code,
                                       ClientManager* clientManager,
                                       QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::TradeHistoryDialog)
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

TradeHistoryDialog::~TradeHistoryDialog() = default;

void TradeHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for trade: "
                               << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    trading::messaging::get_trade_history_request request;
    request.id = boost::uuids::to_string(id_);

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        versions_ = std::move(response.versions);
        historyLoaded();
    });
}

int TradeHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow TradeHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.identity.version,
            .cells = {relative_time_helper::format(version.audit.recorded_at),
                      QString::fromStdString(version.audit.modified_by),
                      QString::fromStdString(version.audit.performed_by),
                      QString::fromStdString(version.audit.change_commentary)}};
}

QString TradeHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::DiffResult
TradeHistoryDialog::calculateDiffAt(int current_index,
                                    int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "External ID", current.identity.external_id,
                previous.identity.external_id);
    checkString(diffs, "Trade Type", current.classification.trade_type,
                previous.classification.trade_type);
    checkString(diffs, "Lifecycle Event",
                current.classification.activity_type_code,
                previous.classification.activity_type_code);
    checkString(diffs, "Netting Set", current.classification.netting_set_id,
                previous.classification.netting_set_id);
    checkString(diffs, "Trade Date", current.lifecycle.trade_date,
                previous.lifecycle.trade_date);
    checkString(diffs, "Effective Date", current.lifecycle.effective_date,
                previous.lifecycle.effective_date);
    checkString(diffs, "Termination Date", current.lifecycle.termination_date,
                previous.lifecycle.termination_date);
    checkString(diffs, "Execution Timestamp",
                current.lifecycle.execution_timestamp,
                previous.lifecycle.execution_timestamp);

    return diffs;
}

void TradeHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->externalIdValue->setText(
        QString::fromStdString(version.identity.external_id));
    ui_->tradeTypeValue->setText(
        QString::fromStdString(version.classification.trade_type));
    ui_->lifecycleEventValue->setText(
        QString::fromStdString(version.classification.activity_type_code));
    ui_->nettingSetIdValue->setText(
        QString::fromStdString(version.classification.netting_set_id));
    ui_->tradeDateValue->setText(optionalText(version.lifecycle.trade_date));
    ui_->effectiveDateValue->setText(
        optionalText(version.lifecycle.effective_date));
    ui_->terminationDateValue->setText(
        optionalText(version.lifecycle.termination_date));
    ui_->executionTimestampValue->setText(
        optionalText(version.lifecycle.execution_timestamp));
    ui_->versionNumberValue->setText(QString::number(version.identity.version));
    ui_->modifiedByValue->setText(
        QString::fromStdString(version.audit.modified_by));
    ui_->recordedAtValue->setText(
        relative_time_helper::format(version.audit.recorded_at));
    ui_->changeCommentaryValue->setText(
        QString::fromStdString(version.audit.change_commentary));
}

void TradeHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening trade version "
                              << version.identity.version
                              << " in read-only mode";
    emit openVersionRequested(version, version.identity.version);
}

void TradeHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; revert TO the
    // selected version.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version "
                              << selected.identity.version;

    emit revertVersionRequested(selected);
}

}
