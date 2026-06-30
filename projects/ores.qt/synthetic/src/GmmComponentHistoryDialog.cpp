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
#include "ores.qt/GmmComponentHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ui_GmmComponentHistoryDialog.h"
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

GmmComponentHistoryDialog::GmmComponentHistoryDialog(const boost::uuids::uuid& id,
                                                     const QString& code,
                                                     ClientManager* clientManager,
                                                     QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::GmmComponentHistoryDialog)
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

GmmComponentHistoryDialog::~GmmComponentHistoryDialog() {
    delete ui_;
}

QString GmmComponentHistoryDialog::code() const {
    return code_;
}

void GmmComponentHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for GMM component: " << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    synthetic::messaging::get_gmm_component_history_request request;
    request.id = boost::uuids::to_string(id_);

    QPointer<GmmComponentHistoryDialog> self = this;
    runHistoryRequest(clientManager_,
                      std::move(request),
                      [self](synthetic::messaging::get_gmm_component_history_response response) {
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

int GmmComponentHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

QString GmmComponentHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::VersionRow GmmComponentHistoryDialog::versionRow(int index) const {
    const auto& v = versions_[index];
    return {v.version,
            {relative_time_helper::format(v.recorded_at),
             QString::fromStdString(v.modified_by),
             QString::fromStdString(v.performed_by),
             QString::fromStdString(v.change_commentary)}};
}

HistoryDialogBase::DiffResult GmmComponentHistoryDialog::calculateDiffAt(int ci, int pi) const {
    DiffResult diffs;
    const auto& curr = versions_[ci];
    const auto& prev = versions_[pi];

    checkInt(diffs, tr("Component Index"), curr.component_index, prev.component_index);
    checkString(diffs, tr("Description"), curr.description, prev.description);
    checkDouble(diffs, tr("Mean"), curr.mean, prev.mean);
    checkDouble(diffs, tr("Std Dev"), curr.stdev, prev.stdev);
    checkDouble(diffs, tr("Weight"), curr.weight, prev.weight);
    return diffs;
}

void GmmComponentHistoryDialog::displayFullDetails(int index) {
    if (index < 0 || static_cast<size_t>(index) >= versions_.size())
        return;

    const auto& version = versions_[index];

    ui_->componentIndexValue->setText(QString::number(version.component_index));
    ui_->descriptionValue->setText(QString::fromStdString(version.description));
    ui_->meanValue->setText(QString::number(version.mean));
    ui_->stdevValue->setText(QString::number(version.stdev));
    ui_->weightValue->setText(QString::number(version.weight));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void GmmComponentHistoryDialog::openVersionAt(int index) {
    emit openVersionRequested(versions_[index], versions_[index].version);
}

void GmmComponentHistoryDialog::revertToVersionAt(int index) {
    emit revertVersionRequested(versions_[index]);
}

}
