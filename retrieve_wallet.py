import sys
from web3 import Web3
from eth_account import Account

def is_hex_string(s):
    try:
        int(s, 16)
        return True
    except ValueError:
        return False

def get_ethereum_address(private_key):
    w3 = Web3()

    # Check if the provided private key is a valid hexadecimal string
    if not is_hex_string(private_key) or len(private_key) != 64:
        print("Invalid private key")
        return None

    # Derive the Ethereum address from the private key
    account = Account.from_key(private_key)
    address = account.address

    return address

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python script_name.py <private_key>")
        sys.exit(1)

    private_key = sys.argv[1]

    # Get and print the Ethereum address
    address = get_ethereum_address(private_key)
    if address:
        print(f"The Ethereum address corresponding to the private key is: {address}")
