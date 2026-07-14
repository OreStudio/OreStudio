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
#include "ores.qt/CounterpartyContactInformationHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/counterparty_contact_information_protocol.hpp"
#include "ui_CounterpartyContactInformationHistoryDialog.h"
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

CounterpartyContactInformationHistoryDialog::CounterpartyContactInformationHistoryDialog(
    const boost::uuids::uuid& id,
    const QString& code,
    ClientManager* clientManager,
    QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::CounterpartyContactInformationHistoryDialog)
    , id_(id)
    , code_(code)
    , clientManager_(clientManager) {

    ui_->setupUi(this);
    ui_->versionListWidget->setColumnCount(5);
    ui_->versionListWidget->setHorizontalHeaderLabels({tr("Version"),
                                                       tr("Recorded At"),
                                                       tr("Modified By"),
                                                       tr("Performed By"),
                                                       tr("Commentary")});
    ui_->changesTableWidget->setColumnCount(3);
    ui_->changesTableWidget->setHorizontalHeaderLabels(
        {tr("Field"), tr("Old Value"), tr("New Value")});
    initializeHistoryUi(
        {ui_->versionListWidget, ui_->changesTableWidget, ui_->titleLabel, ui_->closeButton});
}

CounterpartyContactInformationHistoryDialog::~CounterpartyContactInformationHistoryDialog() {
    delete ui_;
}

QString CounterpartyContactInformationHistoryDialog::code() const {
    return code_;
}

void CounterpartyContactInformationHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for counterparty contact information: "
                               << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_counterparty_contact_information_history_request request;
    request.id = boost::uuids::to_string(id_);

    QPointer<CounterpartyContactInformationHistoryDialog> self = this;
    runHistoryRequest(
        clientManager_,
        std::move(request),
        [self](refdata::messaging::get_counterparty_contact_information_history_response response) {
            if (!self)
                return;
            if (!response.success) {
                self->historyLoadFailed(QString::fromStdString(response.message));
                return;
            }
            self->versions_ = std::move(response.history);
            self->historyLoaded();
        });
}

int CounterpartyContactInformationHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

QString CounterpartyContactInformationHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::VersionRow
CounterpartyContactInformationHistoryDialog::versionRow(int index) const {
    const auto& v = versions_[index];
    return {v.version,
            {relative_time_helper::format(v.recorded_at),
             QString::fromStdString(v.modified_by),
             QString::fromStdString(v.performed_by),
             QString::fromStdString(v.change_commentary)}};
}

HistoryDialogBase::DiffResult
CounterpartyContactInformationHistoryDialog::calculateDiffAt(int ci, int pi) const {
    DiffResult diffs;
    const auto& curr = versions_[ci];
    const auto& prev = versions_[pi];

    if (curr.id != prev.id)
        diffs.append({tr("Id"),
                      {QString::fromStdString(boost::uuids::to_string(prev.id)),
                       QString::fromStdString(boost::uuids::to_string(curr.id))}});
    checkString(diffs, tr("Type"), curr.contact_type, prev.contact_type);
    checkString(diffs, tr("Street Line 1"), curr.street_line_1, prev.street_line_1);
    checkString(diffs, tr("Street Line 2"), curr.street_line_2, prev.street_line_2);
    checkString(diffs, tr("City"), curr.city, prev.city);
    checkString(diffs, tr("State"), curr.state, prev.state);
    checkString(diffs, tr("Country Code"), curr.country_code, prev.country_code);
    checkString(diffs, tr("Postal Code"), curr.postal_code, prev.postal_code);
    checkString(diffs, tr("Phone"), curr.phone, prev.phone);
    checkString(diffs, tr("Email"), curr.email, prev.email);
    checkString(diffs, tr("Web Page"), curr.web_page, prev.web_page);
    return diffs;
}

void CounterpartyContactInformationHistoryDialog::displayFullDetails(int index) {
    if (index < 0 || static_cast<size_t>(index) >= versions_.size())
        return;

    const auto& version = versions_[index];

    ui_->idValue->setText(QString::fromStdString(boost::uuids::to_string(version.id)));
    ui_->contactTypeValue->setText(QString::fromStdString(version.contact_type));
    ui_->streetLine1Value->setText(QString::fromStdString(version.street_line_1));
    ui_->streetLine2Value->setText(QString::fromStdString(version.street_line_2));
    ui_->cityValue->setText(QString::fromStdString(version.city));
    ui_->stateValue->setText(QString::fromStdString(version.state));
    ui_->countryCodeValue->setText(QString::fromStdString(version.country_code));
    ui_->postalCodeValue->setText(QString::fromStdString(version.postal_code));
    ui_->phoneValue->setText(QString::fromStdString(version.phone));
    ui_->emailValue->setText(QString::fromStdString(version.email));
    ui_->webPageValue->setText(QString::fromStdString(version.web_page));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void CounterpartyContactInformationHistoryDialog::openVersionAt(int index) {
    emit openVersionRequested(versions_[index], versions_[index].version);
}

void CounterpartyContactInformationHistoryDialog::revertToVersionAt(int index) {
    emit revertVersionRequested(versions_[index]);
}

}
