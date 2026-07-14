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
#include "ores.qt/BookDetailDialog.hpp"
#include "ores.qt/BadgeComboHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ui_BookDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

BookDetailDialog::BookDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::BookDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupCombos();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
}

BookDetailDialog::~BookDetailDialog() {
    delete ui_;
}

QTabWidget* BookDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* BookDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* BookDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString BookDetailDialog::code() const {
    return QString::fromStdString(book_.name);
}

void BookDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void BookDetailDialog::setupCombos() {}

void BookDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &BookDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &BookDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &BookDetailDialog::onCloseClicked);

    connect(ui_->idEdit, &QLineEdit::textChanged, this, &BookDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this, &BookDetailDialog::onFieldChanged);
    connect(ui_->functionalCurrencyEdit,
            &QComboBox::currentIndexChanged,
            this,
            &BookDetailDialog::onFieldChanged);
    connect(
        ui_->glAccountRefEdit, &QLineEdit::textChanged, this, &BookDetailDialog::onFieldChanged);
    connect(ui_->costCenterEdit, &QLineEdit::textChanged, this, &BookDetailDialog::onFieldChanged);
    connect(ui_->bookStatusCombo,
            &QComboBox::currentIndexChanged,
            this,
            &BookDetailDialog::onFieldChanged);
    connect(ui_->regulatoryBookTypeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &BookDetailDialog::onFieldChanged);
    connect(ui_->isSweepableCheckBox, &QCheckBox::toggled, this, &BookDetailDialog::onFieldChanged);
    connect(ui_->ratesCentreCodeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &BookDetailDialog::onFieldChanged);
}

void BookDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateBookStatusCombo();
    setup_badge_combo(this, ui_->bookStatusCombo, badgeCache(), "book_status");
    populateRegulatoryBookTypeCombo();
    setup_badge_combo(this, ui_->regulatoryBookTypeCombo, badgeCache(), "regulatory_book_type");
    populateFunctionalCurrencyCombo();
    populateRatesCentreCodeCombo();
}

void BookDetailDialog::populateFunctionalCurrencyCombo() {
    setup_currency_combo(ui_->functionalCurrencyEdit, this, clientManager_, imageCache(), [this]() {
        return QString::fromStdString(book_.functional_currency);
    });
}

void BookDetailDialog::populateRatesCentreCodeCombo() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<BookDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    QObject::connect(
        watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
            auto codes = watcher->result();
            watcher->deleteLater();
            if (!self)
                return;

            auto* combo = self->ui_->ratesCentreCodeCombo;
            const QString previous = combo->currentText();
            combo->blockSignals(true);
            combo->clear();
            for (const auto& c : codes)
                combo->addItem(QString::fromStdString(c));
            // fallback_selection is evaluated here (fetch-completion time), not
            // at populate-call time, since setBook() may run before or
            // after setClientManager() triggers this fetch.
            const QString fallback = QString::fromStdString(self->book_.rates_centre_code);
            const QString to_select = !previous.isEmpty() ? previous : fallback;
            if (!to_select.isEmpty()) {
                const int idx = combo->findText(to_select);
                if (idx >= 0)
                    combo->setCurrentIndex(idx);
            }
            combo->blockSignals(false);

            if (self->imageCache())
                apply_flag_icons(
                    combo, self->imageCache(), FlagSource::BusinessCentre, single_flag_icon_size());
        });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() { return fetch_business_centre_codes(cm); }));
}

void BookDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BookDetailDialog::setBook(const refdata::domain::book& book) {
    book_ = book;
    updateUiFromBook();
}

void BookDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        book_.id = boost::uuids::random_generator()();
        if (clientManager_)
            book_.party_id = clientManager_->currentPartyId();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void BookDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BookDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->functionalCurrencyEdit->setEnabled(!readOnly);
    ui_->glAccountRefEdit->setReadOnly(readOnly);
    ui_->costCenterEdit->setReadOnly(readOnly);
    ui_->bookStatusCombo->setEnabled(!readOnly);
    ui_->regulatoryBookTypeCombo->setEnabled(!readOnly);
    ui_->ratesCentreCodeCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BookDetailDialog::populateBookStatusCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating book_status combo";
    populateDynamicCombo<refdata::domain::book_status>(
        ui_->bookStatusCombo,
        this,
        clientManager_,
        &fetch_book_statuses,
        "bookStatusWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(book_.book_status); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load book statuses: %1").arg(error));
        },
        [this]() { setup_badge_combo(this, ui_->bookStatusCombo, badgeCache(), "book_status"); },
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); });
}
void BookDetailDialog::populateRegulatoryBookTypeCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating regulatory_book_type combo";
    populateDynamicCombo<refdata::domain::regulatory_book_type>(
        ui_->regulatoryBookTypeCombo,
        this,
        clientManager_,
        &fetch_regulatory_book_types,
        "regulatoryBookTypeWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(book_.regulatory_book_type); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load regulatory book types: %1").arg(error));
        },
        [this]() {
            setup_badge_combo(
                this, ui_->regulatoryBookTypeCombo, badgeCache(), "regulatory_book_type");
        },
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); });
}
void BookDetailDialog::updateUiFromBook() {
    ui_->idEdit->setText(QString::fromStdString(boost::uuids::to_string(book_.id)));
    ui_->nameEdit->setText(QString::fromStdString(book_.name));
    {
        const auto val = QString::fromStdString(book_.functional_currency);
        const int idx = ui_->functionalCurrencyEdit->findText(val);
        ui_->functionalCurrencyEdit->setCurrentIndex(idx);
    }
    ui_->glAccountRefEdit->setText(QString::fromStdString(book_.gl_account_ref));
    ui_->costCenterEdit->setText(QString::fromStdString(book_.cost_center));
    {
        const auto val = QString::fromStdString(book_.book_status);
        const int idx = ui_->bookStatusCombo->findData(val);
        if (idx >= 0)
            ui_->bookStatusCombo->setCurrentIndex(idx);
    }
    {
        const auto val = QString::fromStdString(book_.regulatory_book_type);
        const int idx = ui_->regulatoryBookTypeCombo->findData(val);
        if (idx >= 0)
            ui_->regulatoryBookTypeCombo->setCurrentIndex(idx);
    }
    ui_->isSweepableCheckBox->setChecked(book_.is_sweepable);
    {
        const auto val = QString::fromStdString(book_.rates_centre_code);
        const int idx = ui_->ratesCentreCodeCombo->findText(val);
        ui_->ratesCentreCodeCombo->setCurrentIndex(idx);
    }

    populateProvenance(book_.version,
                       book_.modified_by,
                       book_.performed_by,
                       book_.recorded_at,
                       book_.change_reason_code,
                       book_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BookDetailDialog::updateBookFromUi() {
    book_.name = ui_->nameEdit->text().trimmed().toStdString();
    book_.functional_currency = ui_->functionalCurrencyEdit->currentText().toStdString();
    book_.gl_account_ref = ui_->glAccountRefEdit->text().trimmed().toStdString();
    book_.cost_center = ui_->costCenterEdit->text().trimmed().toStdString();
    book_.book_status = ui_->bookStatusCombo->currentText().toStdString();
    book_.regulatory_book_type = ui_->regulatoryBookTypeCombo->currentText().toStdString();
    book_.is_sweepable = ui_->isSweepableCheckBox->isChecked();
    book_.rates_centre_code = ui_->ratesCentreCodeCombo->currentText().toStdString();
    book_.modified_by = username_;
}

void BookDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BookDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BookDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool BookDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !id_val.isEmpty() && !name_val.isEmpty();
}

void BookDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save book while disconnected from server.");
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
    book_.change_reason_code = crSel->reason_code;
    book_.change_commentary = crSel->commentary;

    updateBookFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving book: " << book_.name;

    QPointer<BookDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, book = book_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_book_request request;
        request.data = book;
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
            BOOST_LOG_SEV(lg(), info) << "Book saved successfully";
            QString code = QString::fromStdString(self->book_.name);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->bookSaved(code);
            self->notifySaveSuccess(tr("Book '%1' saved").arg(code));
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

void BookDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete book while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(book_.name);
    auto reply =
        MessageBoxHelper::question(this,
                                   "Delete Book",
                                   QString("Are you sure you want to delete book '%1'?").arg(code),
                                   QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting book: " << book_.name;

    QPointer<BookDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(book_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_book_request request;
        request.ids = {id_str};
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
            BOOST_LOG_SEV(lg(), info) << "Book deleted successfully";
            emit self->statusMessage(QString("Book '%1' deleted").arg(code));
            emit self->bookDeleted(code);
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
