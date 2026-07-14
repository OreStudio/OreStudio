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
#include "ores.qt/BusinessCentreHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/business_centre_protocol.hpp"
#include "ui_BusinessCentreHistoryDialog.h"

namespace ores::qt {

using namespace ores::logging;

BusinessCentreHistoryDialog::BusinessCentreHistoryDialog(const QString& code,
                                                         ClientManager* clientManager,
                                                         QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::BusinessCentreHistoryDialog)
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
    initializeHistoryUi({.versionList = ui_->versionListWidget,
                        .changesTable = ui_->changesTableWidget,
                        .titleLabel = ui_->titleLabel,
                        .closeButton = ui_->closeButton});
}

BusinessCentreHistoryDialog::~BusinessCentreHistoryDialog() {
    delete ui_;
}

QString BusinessCentreHistoryDialog::code() const {
    return code_;
}

void BusinessCentreHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for business centre: " << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_business_centre_history_request request;
    request.code = code_.toStdString();

    QPointer<BusinessCentreHistoryDialog> self = this;
    runHistoryRequest(clientManager_,
                      std::move(request),
                      [self](refdata::messaging::get_business_centre_history_response response) {
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

int BusinessCentreHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

QString BusinessCentreHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::VersionRow BusinessCentreHistoryDialog::versionRow(int index) const {
    const auto& v = versions_[index];
    return {v.version,
            {relative_time_helper::format(v.recorded_at),
             QString::fromStdString(v.modified_by),
             QString::fromStdString(v.performed_by),
             QString::fromStdString(v.change_commentary)}};
}

HistoryDialogBase::DiffResult BusinessCentreHistoryDialog::calculateDiffAt(int ci, int pi) const {
    DiffResult diffs;
    const auto& curr = versions_[ci];
    const auto& prev = versions_[pi];

    checkString(diffs, tr("Code"), curr.code, prev.code);
    checkString(diffs, tr("Source"), curr.source, prev.source);
    checkString(diffs, tr("Description"), curr.description, prev.description);
    checkString(diffs, tr("City"), curr.city_name, prev.city_name);
    checkString(diffs, tr("Coding Scheme"), curr.coding_scheme_code, prev.coding_scheme_code);
    checkString(diffs, tr("Country"), curr.country_alpha2_code, prev.country_alpha2_code);
    return diffs;
}

void BusinessCentreHistoryDialog::displayFullDetails(int index) {
    if (index < 0 || static_cast<size_t>(index) >= versions_.size())
        return;

    const auto& version = versions_[index];

    ui_->codeValue->setText(QString::fromStdString(version.code));
    ui_->sourceValue->setText(QString::fromStdString(version.source));
    ui_->descriptionValue->setText(QString::fromStdString(version.description));
    ui_->cityNameValue->setText(QString::fromStdString(version.city_name));
    ui_->codingSchemeValue->setText(QString::fromStdString(version.coding_scheme_code));
    ui_->countryAlpha2Value->setText(QString::fromStdString(version.country_alpha2_code));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void BusinessCentreHistoryDialog::openVersionAt(int index) {
    emit openVersionRequested(versions_[index], versions_[index].version);
}

void BusinessCentreHistoryDialog::revertToVersionAt(int index) {
    emit revertVersionRequested(versions_[index]);
}

}
