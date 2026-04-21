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
#include "ores.qt/ZeroConventionDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_ZeroConventionDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata/messaging/zero_convention_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ZeroConventionDetailDialog::ZeroConventionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::ZeroConventionDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

ZeroConventionDetailDialog::~ZeroConventionDetailDialog() {
    delete ui_;
}

QTabWidget* ZeroConventionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* ZeroConventionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* ZeroConventionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void ZeroConventionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void ZeroConventionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &ZeroConventionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &ZeroConventionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &ZeroConventionDetailDialog::onCloseClicked);

    connect(ui_->idEdit, &QLineEdit::textChanged, this,
            &ZeroConventionDetailDialog::onCodeChanged);
    connect(ui_->dayCountFractionEdit, &QLineEdit::textChanged, this,
            &ZeroConventionDetailDialog::onFieldChanged);
    connect(ui_->compoundingEdit, &QLineEdit::textChanged, this,
            &ZeroConventionDetailDialog::onFieldChanged);
    connect(ui_->compoundingFrequencyEdit, &QLineEdit::textChanged, this,
            &ZeroConventionDetailDialog::onFieldChanged);
    connect(ui_->tenorCalendarEdit, &QLineEdit::textChanged, this,
            &ZeroConventionDetailDialog::onFieldChanged);
    connect(ui_->spotCalendarEdit, &QLineEdit::textChanged, this,
            &ZeroConventionDetailDialog::onFieldChanged);
    connect(ui_->rollConventionEdit, &QLineEdit::textChanged, this,
            &ZeroConventionDetailDialog::onFieldChanged);
}

void ZeroConventionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void ZeroConventionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void ZeroConventionDetailDialog::setConvention(
    const refdata::domain::zero_convention& zc) {
    zc_ = zc;
    updateUiFromConvention();
}

void ZeroConventionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void ZeroConventionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->dayCountFractionEdit->setReadOnly(readOnly);
    ui_->compoundingEdit->setReadOnly(readOnly);
    ui_->compoundingFrequencyEdit->setReadOnly(readOnly);
    ui_->tenorCalendarEdit->setReadOnly(readOnly);
    ui_->spotCalendarEdit->setReadOnly(readOnly);
    ui_->rollConventionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void ZeroConventionDetailDialog::updateUiFromConvention() {
    ui_->idEdit->setText(QString::fromStdString(zc_.id));
    ui_->dayCountFractionEdit->setText(QString::fromStdString(zc_.day_count_fraction));
    ui_->compoundingEdit->setText(QString::fromStdString(zc_.compounding));
    ui_->compoundingFrequencyEdit->setText(QString::fromStdString(zc_.compounding_frequency));
    ui_->tenorCalendarEdit->setText(QString::fromStdString(zc_.tenor_calendar));
    ui_->spotCalendarEdit->setText(QString::fromStdString(zc_.spot_calendar));
    ui_->rollConventionEdit->setText(QString::fromStdString(zc_.roll_convention));

    populateProvenance(zc_.version,
                       zc_.modified_by,
                       zc_.performed_by,
                       zc_.recorded_at,
                       zc_.change_reason_code,
                       zc_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void ZeroConventionDetailDialog::updateConventionFromUi() {
    if (createMode_) {
        zc_.id = ui_->idEdit->text().trimmed().toStdString();
    }
    zc_.day_count_fraction = ui_->dayCountFractionEdit->text().trimmed().toStdString();
    zc_.compounding = ui_->compoundingEdit->text().trimmed().toStdString();
    zc_.compounding_frequency = ui_->compoundingFrequencyEdit->text().trimmed().toStdString();
    zc_.tenor_calendar = ui_->tenorCalendarEdit->text().trimmed().toStdString();
    zc_.spot_calendar = ui_->spotCalendarEdit->text().trimmed().toStdString();
    zc_.roll_convention = ui_->rollConventionEdit->text().trimmed().toStdString();
    zc_.modified_by = username_;
}

void ZeroConventionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ZeroConventionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ZeroConventionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool ZeroConventionDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString day_count_fraction_val = ui_->dayCountFractionEdit->text().trimmed();

    return true
        && !id_val.isEmpty()
        && !day_count_fraction_val.isEmpty()
    ;
}

void ZeroConventionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save zero convention while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateConventionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving zero convention: "
        << zc_.id;

    QPointer<ZeroConventionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, zc = zc_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_zero_convention_request request;
        request.data = zc;
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Zero Convention saved successfully";
            QString code = QString::fromStdString(
                self->zc_.id);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->zcSaved(code);
            self->notifySaveSuccess(tr("Zero Convention '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void ZeroConventionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete zero convention while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(
        zc_.id);
    auto reply = MessageBoxHelper::question(this, "Delete Zero Convention",
        QString("Are you sure you want to delete zero convention '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting zero convention: "
        << zc_.id;

    QPointer<ZeroConventionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = zc_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_zero_convention_request request;
        request.codes = {code};
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Zero Convention deleted successfully";
            emit self->statusMessage(
                QString("Zero Convention '%1' deleted").arg(code));
            emit self->zcDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
