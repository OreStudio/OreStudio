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
#include "ores.qt/CounterpartyHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ui_CounterpartyHistoryDialog.h"
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

CounterpartyHistoryDialog::CounterpartyHistoryDialog(const boost::uuids::uuid& id,
                                                     const QString& code,
                                                     ClientManager* clientManager,
                                                     QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::CounterpartyHistoryDialog)
    , id_(id)
    , code_(code)
    , clientManager_(clientManager) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    ui_->titleLabel->setText(QString("History for: %1").arg(code_));

    ui_->versionListWidget->setColumnCount(5);
    ui_->versionListWidget->setHorizontalHeaderLabels(
        {"Version", "Recorded At", "Modified By", "Performed By", "Commentary"});

    initializeHistoryUi({.versionList = ui_->versionListWidget,
                         .changesTable = ui_->changesTableWidget,
                         .titleLabel = ui_->titleLabel,
                         .closeButton = ui_->closeButton});
}

CounterpartyHistoryDialog::~CounterpartyHistoryDialog() = default;

void CounterpartyHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for counterparty: " << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_counterparty_history_request request;
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

int CounterpartyHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow CounterpartyHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.version,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.performed_by),
                      QString::fromStdString(version.change_commentary)}};
}

QString CounterpartyHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::DiffResult CounterpartyHistoryDialog::calculateDiffAt(int current_index,
                                                                         int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Short Code", current.short_code, previous.short_code);
    checkString(diffs, "Full Name", current.full_name, previous.full_name);

    if (current.transliterated_name.value_or("") != previous.transliterated_name.value_or("")) {
        diffs.append({"Transliterated Name",
                      {QString::fromStdString(previous.transliterated_name.value_or("")),
                       QString::fromStdString(current.transliterated_name.value_or(""))}});
    }

    checkString(diffs, "Party Type", current.party_type, previous.party_type);
    checkString(diffs, "Status", current.status, previous.status);
    checkString(
        diffs, "Business Center", current.business_center_code, previous.business_center_code);

    return diffs;
}

void CounterpartyHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->codeValue->setText(QString::fromStdString(version.short_code));
    ui_->nameValue->setText(QString::fromStdString(version.full_name));
    ui_->transliteratedNameValue->setText(
        QString::fromStdString(version.transliterated_name.value_or("")));
    ui_->partyTypeValue->setText(QString::fromStdString(version.party_type));
    ui_->statusValue->setText(QString::fromStdString(version.status));
    ui_->businessCenterValue->setText(QString::fromStdString(version.business_center_code));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));

    loadCompositeAsOf(version.version);
}

void CounterpartyHistoryDialog::loadCompositeAsOf(int version_number) {
    ui_->compositeIdentifiersListWidget->clear();
    ui_->compositeContactsListWidget->clear();
    ui_->compositeIdentifiersListWidget->addItem("Loading...");

    refdata::messaging::get_counterparty_composite_as_of_request request;
    request.id = boost::uuids::to_string(id_);
    request.version = version_number;

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        ui_->compositeIdentifiersListWidget->clear();
        ui_->compositeContactsListWidget->clear();

        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Composite as-of request failed: " << response.message;
            ui_->compositeIdentifiersListWidget->addItem(
                QString("Failed to load: %1").arg(QString::fromStdString(response.message)));
            return;
        }

        if (response.identifiers.empty()) {
            ui_->compositeIdentifiersListWidget->addItem("(none as of this version)");
        }
        for (const auto& pi : response.identifiers) {
            ui_->compositeIdentifiersListWidget->addItem(
                QString("%1: %2 (v%3)")
                    .arg(QString::fromStdString(pi.id_scheme))
                    .arg(QString::fromStdString(pi.id_value))
                    .arg(pi.version));
        }

        if (response.contacts.empty()) {
            ui_->compositeContactsListWidget->addItem("(none as of this version)");
        }
        for (const auto& c : response.contacts) {
            ui_->compositeContactsListWidget->addItem(
                QString("%1 (v%2)").arg(QString::fromStdString(c.contact_type)).arg(c.version));
        }
    });
}

void CounterpartyHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening counterparty version " << version.version
                              << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void CounterpartyHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; the server handles
    // versioning.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version " << selected.version;

    emit revertVersionRequested(selected);
}

}
