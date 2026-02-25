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
#include "ores.qt/BusinessCentreDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_BusinessCentreDetailDialog.h"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata/messaging/business_centre_protocol.hpp"
#include "ores.refdata/messaging/country_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

BusinessCentreDetailDialog::BusinessCentreDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::BusinessCentreDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupConnections();
}

BusinessCentreDetailDialog::~BusinessCentreDetailDialog() {
    delete ui_;
}

QTabWidget* BusinessCentreDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* BusinessCentreDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* BusinessCentreDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void BusinessCentreDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void BusinessCentreDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &BusinessCentreDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &BusinessCentreDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &BusinessCentreDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &BusinessCentreDetailDialog::onCodeChanged);
    connect(ui_->sourceEdit, &QLineEdit::textChanged, this,
            &BusinessCentreDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &BusinessCentreDetailDialog::onFieldChanged);
    connect(ui_->codingSchemeEdit, &QLineEdit::textChanged, this,
            &BusinessCentreDetailDialog::onFieldChanged);
    connect(ui_->countryAlpha2Combo, &QComboBox::currentTextChanged, this,
            &BusinessCentreDetailDialog::onFieldChanged);
}

void BusinessCentreDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateCountries();
}

void BusinessCentreDetailDialog::populateCountries() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<BusinessCentreDetailDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm]() -> std::vector<std::string> {
        refdata::messaging::get_countries_request request;
        request.limit = 1000;
        auto response = cm->process_authenticated_request(std::move(request));
        if (!response) return {};

        std::vector<std::string> codes;
        codes.reserve(response->countries.size());
        for (const auto& country : response->countries) {
            codes.push_back(country.alpha2_code);
        }
        return codes;
    };

    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished,
            self, [self, watcher]() {
        auto codes = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        self->ui_->countryAlpha2Combo->blockSignals(true);
        self->ui_->countryAlpha2Combo->clear();
        self->ui_->countryAlpha2Combo->addItem(QString());
        for (const auto& code : codes) {
            self->ui_->countryAlpha2Combo->addItem(
                QString::fromStdString(code));
        }
        self->ui_->countryAlpha2Combo->blockSignals(false);

        apply_flag_icons(self->ui_->countryAlpha2Combo, self->imageCache_,
                         FlagSource::Country);
        self->updateUiFromBusinessCentre();

        // Apply the pending country selection now that the combo is populated.
        if (!self->pending_country_.empty()) {
            self->ui_->countryAlpha2Combo->setCurrentText(
                QString::fromStdString(self->pending_country_));
        }
    });

    QFuture<std::vector<std::string>> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void BusinessCentreDetailDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
    setup_flag_combo(this, ui_->countryAlpha2Combo, imageCache_, FlagSource::Country);
}

void BusinessCentreDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BusinessCentreDetailDialog::setBusinessCentre(
    const refdata::domain::business_centre& business_centre) {
    business_centre_ = business_centre;
    pending_country_ = business_centre.country_alpha2_code;
    updateUiFromBusinessCentre();
    // If the combo already has items (populated before this call), apply now.
    if (ui_->countryAlpha2Combo->count() > 0) {
        ui_->countryAlpha2Combo->setCurrentText(
            QString::fromStdString(pending_country_));
    }
}

void BusinessCentreDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    setProvenanceEnabled(!createMode);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BusinessCentreDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->cityNameEdit->setReadOnly(true);
    ui_->sourceEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->codingSchemeEdit->setReadOnly(readOnly);
    ui_->countryAlpha2Combo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BusinessCentreDetailDialog::updateUiFromBusinessCentre() {
    ui_->codeEdit->setText(QString::fromStdString(business_centre_.code));
    ui_->cityNameEdit->setText(QString::fromStdString(business_centre_.city_name));
    ui_->sourceEdit->setText(QString::fromStdString(business_centre_.source));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(business_centre_.description));
    ui_->codingSchemeEdit->setText(QString::fromStdString(business_centre_.coding_scheme_code));

    populateProvenance(business_centre_.version, business_centre_.modified_by,
                       business_centre_.performed_by, business_centre_.recorded_at,
                       business_centre_.change_reason_code,
                       business_centre_.change_commentary);
    hasChanges_ = false;
    updateSaveButtonState();
}

void BusinessCentreDetailDialog::updateBusinessCentreFromUi() {
    if (createMode_) {
        business_centre_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    business_centre_.source = ui_->sourceEdit->text().trimmed().toStdString();
    business_centre_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    business_centre_.coding_scheme_code = ui_->codingSchemeEdit->text().trimmed().toStdString();
    business_centre_.country_alpha2_code = ui_->countryAlpha2Combo->currentText().trimmed().toStdString();
    business_centre_.modified_by = username_;
    business_centre_.performed_by = username_;
}

void BusinessCentreDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessCentreDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessCentreDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool BusinessCentreDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    return !code_val.isEmpty();
}

void BusinessCentreDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save business centre while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateBusinessCentreFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving business centre: " << business_centre_.code;

    QPointer<BusinessCentreDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [cm = clientManager_, business_centre = business_centre_]() -> SaveResult {
        if (!cm) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_business_centre_request request;
        request.business_centre = business_centre;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_business_centre_request,
            0, std::move(payload)
        );

        auto response_result = cm->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = refdata::messaging::save_business_centre_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Business centre saved successfully";
            QString code = QString::fromStdString(self->business_centre_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->businessCentreSaved(code);
            self->notifySaveSuccess(tr("Business centre '%1' saved").arg(code));
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

void BusinessCentreDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete business centre while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(business_centre_.code);
    auto reply = MessageBoxHelper::question(this, "Delete Business Centre",
        QString("Are you sure you want to delete business centre '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting business centre: " << business_centre_.code;

    QPointer<BusinessCentreDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [cm = clientManager_, code_str = business_centre_.code]() -> DeleteResult {
        if (!cm) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_business_centre_request request;
        request.codes = {code_str};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_business_centre_request,
            0, std::move(payload)
        );

        auto response_result = cm->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = refdata::messaging::delete_business_centre_response::
            deserialize(*payload_result);

        if (!response || response->results.empty()) {
            return {false, "Invalid server response"};
        }

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Business centre deleted successfully";
            emit self->statusMessage(QString("Business centre '%1' deleted").arg(code));
            emit self->businessCentreDeleted(code);
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
