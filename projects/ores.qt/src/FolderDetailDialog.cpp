/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/FolderDetailDialog.hpp"
#include "ores.connections/service/connection_manager.hpp"
#include <QFormLayout>
#include <QVBoxLayout>
#include <QMessageBox>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

FolderDetailDialog::FolderDetailDialog(
    connections::service::connection_manager* manager,
    QWidget* parent)
    : QDialog(parent),
      manager_(manager) {

    setupUI();
}

FolderDetailDialog::~FolderDetailDialog() = default;

void FolderDetailDialog::setupUI() {
    setWindowTitle(tr("New Folder"));
    setMinimumWidth(350);

    auto* layout = new QVBoxLayout(this);

    auto* formLayout = new QFormLayout();

    nameEdit_ = new QLineEdit(this);
    nameEdit_->setPlaceholderText(tr("Enter folder name"));
    formLayout->addRow(tr("Name:"), nameEdit_);

    parentCombo_ = new QComboBox(this);
    formLayout->addRow(tr("Parent Folder:"), parentCombo_);

    layout->addLayout(formLayout);

    buttonBox_ = new QDialogButtonBox(
        QDialogButtonBox::Save | QDialogButtonBox::Cancel, this);
    layout->addWidget(buttonBox_);

    populateParentCombo();

    // Connections
    connect(nameEdit_, &QLineEdit::textChanged,
            this, &FolderDetailDialog::updateSaveButtonState);
    connect(buttonBox_, &QDialogButtonBox::accepted,
            this, &FolderDetailDialog::onSaveClicked);
    connect(buttonBox_, &QDialogButtonBox::rejected,
            this, &QDialog::reject);

    updateSaveButtonState();
}

void FolderDetailDialog::populateParentCombo() {
    using namespace ores::logging;

    parentCombo_->clear();

    // Add "None" option for root level
    parentCombo_->addItem(tr("(Root Level)"), QString());

    try {
        auto folders = manager_->get_all_folders();
        for (const auto& folder : folders) {
            // Skip self when editing
            if (!isAddMode_ && folder.id == folderId_)
                continue;

            parentCombo_->addItem(
                QString::fromStdString(folder.name),
                QString::fromStdString(boost::uuids::to_string(folder.id)));
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load folders: " << e.what();
    }
}

void FolderDetailDialog::setFolder(const connections::domain::folder& folder) {
    using namespace ores::logging;

    isAddMode_ = false;
    folderId_ = folder.id;

    setWindowTitle(tr("Edit Folder"));
    nameEdit_->setText(QString::fromStdString(folder.name));

    // Repopulate to exclude self
    populateParentCombo();

    // Set parent selection
    if (folder.parent_id) {
        QString parentIdStr = QString::fromStdString(
            boost::uuids::to_string(*folder.parent_id));
        int index = parentCombo_->findData(parentIdStr);
        if (index >= 0)
            parentCombo_->setCurrentIndex(index);
    } else {
        parentCombo_->setCurrentIndex(0); // Root level
    }

    BOOST_LOG_SEV(lg(), debug) << "Editing folder: " << folder.name;
}

void FolderDetailDialog::setInitialParent(
    const std::optional<boost::uuids::uuid>& parentId) {

    if (parentId) {
        QString parentIdStr = QString::fromStdString(
            boost::uuids::to_string(*parentId));
        int index = parentCombo_->findData(parentIdStr);
        if (index >= 0)
            parentCombo_->setCurrentIndex(index);
    }
}

connections::domain::folder FolderDetailDialog::getFolder() const {
    connections::domain::folder folder;

    if (isAddMode_) {
        folder.id = boost::uuids::random_generator()();
    } else {
        folder.id = folderId_;
    }

    folder.name = nameEdit_->text().trimmed().toStdString();

    // Get parent ID from combo
    QString parentIdStr = parentCombo_->currentData().toString();
    if (!parentIdStr.isEmpty()) {
        boost::uuids::string_generator gen;
        folder.parent_id = gen(parentIdStr.toStdString());
    }

    return folder;
}

void FolderDetailDialog::onSaveClicked() {
    if (validateInput()) {
        accept();
    }
}

void FolderDetailDialog::updateSaveButtonState() {
    auto* saveButton = buttonBox_->button(QDialogButtonBox::Save);
    saveButton->setEnabled(!nameEdit_->text().trimmed().isEmpty());
}

bool FolderDetailDialog::validateInput() {
    QString name = nameEdit_->text().trimmed();

    if (name.isEmpty()) {
        QMessageBox::warning(this, tr("Validation Error"),
            tr("Folder name cannot be empty."));
        nameEdit_->setFocus();
        return false;
    }

    return true;
}

}
