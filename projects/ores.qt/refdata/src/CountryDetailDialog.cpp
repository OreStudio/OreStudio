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
#include "ores.qt/CountryDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/country_protocol.hpp"
#include "ui_CountryDetailDialog.h"
#include <QFutureWatcher>
#include <QIcon>
#include <QLineEdit>
#include <QMessageBox>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

CountryDetailDialog::CountryDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CountryDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
}

CountryDetailDialog::~CountryDetailDialog() {
    delete ui_;
}

QTabWidget* CountryDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CountryDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CountryDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CountryDetailDialog::code() const {
    return QString::fromStdString(country_.alpha2_code);
}

void CountryDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    // Flag editor hosted in the .ui flagGroup; base class owns the button
    // (also wires the inline key-field icon — see initKeyFlagField()).
    initFlagButton(ui_->flagGroup->layout());
}

std::optional<boost::uuids::uuid> CountryDetailDialog::entityImageId() const {
    return country_.image_id;
}

QLineEdit* CountryDetailDialog::keyFlagField() const {
    return ui_->codeEdit;
}

QIcon CountryDetailDialog::keyFlagIcon(const std::string& key) const {
    return imageCache() ? imageCache()->getCountryFlagIcon(key) : QIcon();
}

void CountryDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &CountryDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &CountryDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &CountryDetailDialog::onCloseClicked);
    connect(this, &DetailDialogBase::flagEdited, this, &CountryDetailDialog::onFieldChanged);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this, &CountryDetailDialog::onCodeChanged);
    connect(
        ui_->alpha3CodeEdit, &QLineEdit::textChanged, this, &CountryDetailDialog::onFieldChanged);
    connect(
        ui_->numericCodeEdit, &QLineEdit::textChanged, this, &CountryDetailDialog::onFieldChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this, &CountryDetailDialog::onFieldChanged);
    connect(
        ui_->officialNameEdit, &QLineEdit::textChanged, this, &CountryDetailDialog::onFieldChanged);
}

void CountryDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CountryDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CountryDetailDialog::setCountry(const refdata::domain::country& country) {
    country_ = country;
    updateUiFromCountry();
}

void CountryDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void CountryDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CountryDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->alpha3CodeEdit->setReadOnly(readOnly);
    ui_->numericCodeEdit->setReadOnly(readOnly);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->officialNameEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CountryDetailDialog::updateUiFromCountry() {
    ui_->codeEdit->setText(QString::fromStdString(country_.alpha2_code));
    ui_->alpha3CodeEdit->setText(QString::fromStdString(country_.alpha3_code));
    ui_->numericCodeEdit->setText(QString::fromStdString(country_.numeric_code));
    ui_->nameEdit->setText(QString::fromStdString(country_.name));
    ui_->officialNameEdit->setText(QString::fromStdString(country_.official_name));

    populateProvenance(country_.version,
                       country_.modified_by,
                       country_.performed_by,
                       country_.recorded_at,
                       country_.change_reason_code,
                       country_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CountryDetailDialog::updateCountryFromUi() {
    if (createMode_) {
        country_.alpha2_code = ui_->codeEdit->text().trimmed().toStdString();
    }
    country_.alpha3_code = ui_->alpha3CodeEdit->text().trimmed().toStdString();
    country_.numeric_code = ui_->numericCodeEdit->text().trimmed().toStdString();
    country_.name = ui_->nameEdit->text().trimmed().toStdString();
    country_.official_name = ui_->officialNameEdit->text().trimmed().toStdString();
    country_.modified_by = username_;
}

void CountryDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CountryDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CountryDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CountryDetailDialog::validateInput() {
    const QString alpha2_code_val = ui_->codeEdit->text().trimmed();
    const QString alpha3_code_val = ui_->alpha3CodeEdit->text().trimmed();
    const QString numeric_code_val = ui_->numericCodeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();
    const QString official_name_val = ui_->officialNameEdit->text().trimmed();

    return true && !alpha2_code_val.isEmpty() && !alpha3_code_val.isEmpty() &&
           !numeric_code_val.isEmpty() && !name_val.isEmpty() && !official_name_val.isEmpty();
}

void CountryDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save country while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }


    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    country_.change_reason_code = crSel->reason_code;
    country_.change_commentary = crSel->commentary;
    if (flagChanged())
        country_.image_id = selectedImageId();

    updateCountryFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving country: " << country_.alpha2_code;

    QPointer<CountryDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, country = country_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_country_request request;
        request.data = country;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Country saved successfully";
            QString code = QString::fromStdString(self->country_.alpha2_code);
            self->hasChanges_ = false;
            self->resetFlagChanged();
            self->updateSaveButtonState();
            emit self->countrySaved(code);
            self->notifySaveSuccess(tr("Country '%1' saved").arg(code));
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

void CountryDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete country while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(country_.alpha2_code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Country",
        QString("Are you sure you want to delete country '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting country: " << country_.alpha2_code;

    QPointer<CountryDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = country_.alpha2_code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_country_request request;
        request.alpha2_codes = {code};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Country deleted successfully";
            emit self->statusMessage(QString("Country '%1' deleted").arg(code));
            emit self->countryDeleted(code);
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
